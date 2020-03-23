val optionals : yes ::: {Type} -> no ::: {Type} -> [yes ~ no]
                => folder yes -> folder no
                -> $yes
                -> $(map option (yes ++ no))
