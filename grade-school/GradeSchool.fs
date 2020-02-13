module GradeSchool

type School = Map<int, string list>

let empty : School = Map.empty

let grade (num: int) (school: School): string list = 
    match school |> Map.tryFind num with
    | Some students -> students
    | None -> []

let add (student: string) (inGrade: int) (school: School): School = 
    let current = school |> grade inGrade
    school 
    |> Map.add inGrade ((student :: current) |> List.sort)

let roster (school: School): string list = 
    school
    |> Map.toList
    |> List.sort
    |> List.collect (snd >> List.sort)

