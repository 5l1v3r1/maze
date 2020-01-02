open System

let w = 39
let h = 19
let random = new Random()
let matrix: array<array<char*bool>> = [| for i=0 to h-1 do [|for j=0 to w-1 do yield ('█',false)|]  |]

type coord(t,r,b,l) =
    member val top = t with get
    member val right = r with get
    member val bottom = b with get
    member val left = l with get

    member this.count() =
        let mutable maxDirections = 0
        if t = 1 then
            maxDirections <- maxDirections + 1
        if r = 1 then
            maxDirections <- maxDirections + 1
        if b = 1 then
            maxDirections <- maxDirections + 1
        if l = 1 then
            maxDirections <- maxDirections + 1
        maxDirections

    member this.getCoords() =
        let mutable availableCoords = []
        if this.top <> 0 then
            availableCoords <- "N"::availableCoords
        if this.right <> 0 then
            availableCoords <- "E"::availableCoords
        if this.bottom <> 0 then
            availableCoords <- "O"::availableCoords
        if this.left <> 0 then
            availableCoords <- "S"::availableCoords
        availableCoords

let checkPoint(x,y) =
    x > 0 && x < w - 1 && y > 0 && y < h - 1

let setPath(x,y) =
    matrix.[y].[x] <- (' ',snd(matrix.[y].[x]))

let rec generateMatrix(x,y) =
    if checkPoint(x,y) && fst(matrix.[y].[x]) <> ' ' then
        setPath(x,y)
        generateMatrix(x+2,y)
        generateMatrix(x-2,y)
        generateMatrix(x,y+2)
        generateMatrix(x,y-2)

let isLinked(x,y) =
    snd(matrix.[y].[x])

let legalAdjacents(x,y) =
    let mutable l = 0
    let mutable r = 0
    let mutable t = 0
    let mutable b = 0
    if checkPoint(x+2,y) && isLinked(x+2,y) = false then
        r <- 1
    if checkPoint(x-2,y) && isLinked(x-2,y) = false then
        l <- 1
    if checkPoint(x,y+2) && isLinked(x,y+2) = false then
        t <- 1
    if checkPoint(x,y-2) && isLinked(x,y-2) = false then
        b <- 1
    coord(t,r,b,l)

let setLink(x,y,d) =
    if d = "N" then
        matrix.[y+1].[x] <- (' ',true)
        matrix.[y+2].[x] <- (' ',true)
        (x,y+2)
    else if d = "E" then
        matrix.[y].[x+1] <- (' ',true)
        matrix.[y].[x+2] <- (' ',true)
        (x+2,y)
    else if d = "O" then
        matrix.[y-1].[x] <- (' ',true)
        matrix.[y-2].[x] <- (' ',true)
        (x,y-2)
    else 
        matrix.[y].[x-1] <- (' ',true)
        matrix.[y].[x-2] <- (' ',true)
        (x-2,y)

let rec generateMaze(x,y) =
    let adjs = legalAdjacents(x,y)
    if adjs.count() > 1 then
        let mutable max = random.Next(1,3)
        let coords = adjs.getCoords()
        while max > 0 do
            let p = setLink(x,y,coords.[random.Next(0,coords.Length)])
            generateMaze p
            max <- max - 1
    else if adjs.count() = 1 then 
        let p = setLink(x,y,adjs.getCoords().[0])
        generateMaze p

let endPoint = (random.Next(1,w-1),random.Next(1,h-1))
generateMatrix(fst(endPoint),snd(endPoint))
generateMaze(fst(endPoint),snd(endPoint))

matrix.[snd(endPoint)].[fst(endPoint)] <- ('x',true)

// print matrix
for r in matrix do
    for c in r do
        printf "%c" (fst(c))
    printfn ""