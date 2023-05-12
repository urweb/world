(* For this demo, it's necessary to create chatgptSecrets.ur,
 * defining [api_token]. *)
structure C = Chatgpt.Make(Chatgpt.TwoLegged(ChatgptSecrets))

fun chat log =
    C.Chat.completions {Model = readError "gpt-3.5-turbo",
                        Messages = log}

fun main () =
    log <- source [];
    prompt <- source "";
    return <xml><body>
      <ul>
        <dyn signal={log <- signal log;
                     return (List.mapX (fn msg =>
                                           <xml><li><b>{[msg.Role]}:</b> {[msg.Content]}</li></xml>) log)}/>
      </ul>
      <ctextbox source={prompt}/>
      <button value="Chat" onclick={fn _ => pr <- get prompt;
                                       log' <- get log;
                                       log' <- return (List.append log' ({Role = Chatgpt.User,
                                                                          Content = pr} :: []));
                                       resp <- rpc (chat log');
                                       log' <- return (List.append log' ({Role = Chatgpt.Assistant,
                                                                          Content = resp} :: []));
                                       set log log';
                                       set prompt ""}/>
    </body></xml>
