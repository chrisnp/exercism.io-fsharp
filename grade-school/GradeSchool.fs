module GradeSchool

type School = Map<int, string list>

let empty : School = Map.empty

let grade (number : int) (school : School) : string list = 
    match school.TryFind number with
    | Some students -> students
    | None -> []

let private studentExists (student : string) (school : School) : bool =
    school
    |> Map.exists (fun _ students -> List.contains student students) 

let add (student : string) (inGrade : int) (school : School) : School = 
    let current = school |> grade inGrade
    if school |> studentExists student then
        school
    else
        school
        |> Map.add inGrade ((student :: current) |> List.sort)

let roster (school : School) : string list = 
    school
    |> Map.toSeq
    |> Seq.sort
    |> Seq.collect (snd >> List.sort)
    |> Seq.toList
