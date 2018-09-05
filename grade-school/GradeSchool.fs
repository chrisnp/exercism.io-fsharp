module GradeSchool

type School = Map<int, string list>

let empty : School = Map.empty

let grade (number: int) (school: School): string list = 
    match school |> Map.tryFind number with
    | Some students -> students
    | None -> []
let add (student: string) (grd: int) (school: School): School = 
    let current = school |> grade grd
    school |> Map.add grd ((student :: current) |> List.sort)

let roster (school: School): (int * string list) list = 
    school |> Map.toList |> List.sortBy fst


