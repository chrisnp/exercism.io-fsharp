module SaddlePoints

let point (fn: int list -> int) (m: int list list) =
    [for i in [0..m.Length - 1] do 
        for j in [0..m.[0].Length - 1] do
            if m.[i].[j] = fn m.[i] then
                yield (i + 1, j + 1)]

let saddlePoints (matrix: int list list): (int * int) list =
    if matrix = [[]] then []
    else
        let transposed = List.transpose matrix 
        let maxRow = point List.max matrix 
        let minCol = List.map <|| ((fun (x, y) -> (y, x)), 
                                   (point List.min transposed))
        maxRow |> List.filter (fun x -> List.contains x minCol)
