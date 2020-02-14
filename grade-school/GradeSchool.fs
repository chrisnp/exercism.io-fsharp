module GradeSchool

type School = Map<int, string list>


let empty : School = Map.empty


let grade (number : int) 
          (school : School) : string list = 
    match school.TryFind number with
    | Some students -> students
    | None -> []


let add (student : string) 
        (inGrade : int) 
        (school : School) : School = 
    let current = school |> grade inGrade
    school
    |> Map.add inGrade ((student :: current) 
                        |> List.sort)


let roster (school : School) : string list = 
    school
    |> Map.toSeq
    |> Seq.sort
    |> Seq.collect (snd >> List.sort)
    |> Seq.toList

