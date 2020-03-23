fun optionals [yes ::: {Type}] [no ::: {Type}] [yes ~ no]
              (yfl : folder yes) (nfl : folder no)
              (r : $yes) =
    @mp [ident] [option] (fn [t] => Some) yfl r
     ++ @map0 [option] (fn [t ::_] => None) nfl
