(* For this demo, it's necessary to create claudeSecrets.ur,
 * defining [api_key]. *)
structure C = Claude.Make(Claude.TwoLegged(ClaudeSecrets))

fun render log =
    case log of
        [] => <xml></xml>
      | human :: [] => <xml>
        <li><b>Human:</b> {[human]}</li>
      </xml>
      | human :: assistant :: log' => <xml>
        <li><b>Human:</b> {[human]}</li>
        <li><b>Assistant:</b> {[assistant]}</li>
        {render log'}
      </xml>

fun flatten log =
    case log of
        [] => ""
      | human :: [] =>
        "\n\nHuman: " ^ human ^ "\n\nAssistant:"
      | human :: assistant :: log' =>
        "\n\nHuman: " ^ human ^ "\n\nAssistant: " ^ assistant ^ flatten log'

fun chat log =
    C.complete {Model = readError "claude-2",
                MaxTokensToSample = 1000,
                Prompt = flatten log}

fun main () =
    log <- source [];
    prompt <- source "";
    return <xml><body>
      <ul>
        <dyn signal={log <- signal log;
                     return (render log)}/>
      </ul>
      <ctextbox source={prompt}/>
      <button value="Chat" onclick={fn _ => pr <- get prompt;
                                       log' <- get log;
                                       log' <- return (List.append log' (pr :: []));
                                       resp <- rpc (chat log');
                                       log' <- return (List.append log' (resp :: []));
                                       set log log';
                                       set prompt ""}/>
    </body></xml>
