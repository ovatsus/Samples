
let rSquared modelVariance originalVariance = 
    1.0 - modelVariance / originalVariance

let variance points = 
    let mean = List.sum points / float points.Length
    points |> List.sumBy (fun x -> (x - mean) ** 2.0)

let rSquaredCategoricalModel listsOfPoints =
    let originalVariance = listsOfPoints |> List.reduce (@) |> variance
    let categoriesVariance = listsOfPoints |> List.map variance |> List.sum
    rSquared categoriesVariance originalVariance

rSquaredCategoricalModel [[12.0; 14.0]
                          [12.0;  6.0]]

let rSquaredLinearModel m b points =
    let mean = (points |> List.sumBy (fun (_, y) -> y)) / float points.Length    
    let originalVariance = points |> List.sumBy (fun (x, y) -> (y - mean) ** 2.0)
    let linearModelVariance = points |> List.sumBy (fun (x, y) -> (m * x + b - y) ** 2.0)
    rSquared linearModelVariance originalVariance

rSquaredLinearModel 20.0 0.0 [2.0, 45.0
                              4.0, 80.0
                              7.0, 95.0
                              3.0, 55.0
                              1.0, 30.0]

rSquaredLinearModel 0.5 0.0 [1.0,  5.0
                             25.0, 15.0
                             46.0, 22.0
                             76.0, 32.0
                             140.0, 77.0]

let calcLinearModel (coeffs:float list) values = List.map2 (*) coeffs values |> List.sum

let calcFootbackTeamScore values = calcLinearModel [0.18; 0.25; 0.12] values

calcFootbackTeamScore [7.0; 6.0; 6.0]
calcFootbackTeamScore [9.0; 8.0; 7.0]