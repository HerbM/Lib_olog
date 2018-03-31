open Ologs

let a_person = {default_obj with name = "a person"; aspects = []}

let a_man = {default_obj with name = "a man"; aspects = [Ordinary,Is, a_person]}

let a_father = {default_obj with name = "a father"; aspects = [Ordinary,Is, a_man; Ordinary,Is, a_person]}

let a_woman = "a woman" <$> is_ a_person;;

let a_mother = "a mother" <$> is_ a_woman <*> rel "has a first child" a_person;;

