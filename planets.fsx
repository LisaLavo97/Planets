#load "LWC.fsx"
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open LWC


type MyButton() = 
    inherit LWCControl()

    let mutable text = ""

    member this.Text 
        with get() = text
        and set(v) = text <- v

    override this.OnPaint e =
        let g = e.Graphics
        let parent = this.Parent
        let r = RectF2Rect(RectangleF(this.Position, this.Size)) 
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.FillRectangle(Brushes.YellowGreen, r)
        g.DrawRectangle(Pens.Black, r)

        let textSize = g.MeasureString(text, UserControl.DefaultFont)
        let p = this.Position
        let sz = this.Size
        let sx, sy = p.X + (sz.Width - textSize.Width) / 2.f, p.Y + (sz.Height - textSize.Height) / 2.f
        g.DrawString(text, UserControl.DefaultFont, Brushes.Black, PointF(sx, sy))


type Maniglia() =
    inherit LWCControl()

    let mutable penWidth = 1.f

    member this.PenWidth
        with get() = penWidth
        and set(v) = penWidth <- v

    override this.OnPaint e =
        let g = e.Graphics
        let parent = this.Parent
        let position = PointF(this.Position.X - this.Width/2.f, this.Position.Y - this.Height/2.f)
        let r = RectF2Rect(RectangleF(position, this.Size)) 

        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.DrawEllipse(new Pen(Color.Orange,penWidth), r)

    override this.HitTest p =
        let x = this.Position.X - p.X
        let y = this.Position.Y - p.Y
        (x*x) + (y*y) < (this.Height/2.f) * (this.Height/2.f) 


type Planet() = 
    inherit LWCControl()

    let mutable penWidth = 1.f
    let mutable image: Bitmap option = None
    let mutable mass = 0.f
    let mutable maniglie = ResizeArray<Maniglia>()
    let mutable vectorEnd = PointF(35.f, 35.f)

    member this.PenWidth
        with get() = penWidth
        and set(v) = penWidth <- v

    member this.AddImage img =
        image <- img
    
    member this.Mass
        with get() = mass
        and set(m) = mass <- m

    member  this.Maniglie with get() = maniglie      
    
    member this.VectorEnd 
        with get() = vectorEnd
        and set(v) = vectorEnd <- v

    member this.UpdatePositionManiglie(position: PointF) =
        this.Maniglie.[0].Position <- PointF(position.X, position.Y - this.Height/2.f)
        this.Maniglie.[1].Position <- PointF(position.X, position.Y + this.Height/2.f)
        this.Maniglie.[2].Position <- PointF(position.X - this.Width/2.f, position.Y)
        this.Maniglie.[3].Position <- PointF(position.X + this.Width/2.f, position.Y)
        this.Maniglie.[4].Position <- PointF(position.X + this.VectorEnd.X, position.Y + this.VectorEnd.Y)

    override this.OnPaint e =
        let g = e.Graphics
        let parent = this.Parent
        let position = PointF(this.Position.X - this.Width/2.f, this.Position.Y - this.Height/2.f)
        let r = RectF2Rect(RectangleF(position, this.Size)) 
        use vectorP = new Pen(Color.Green, 1.f)
        vectorP.EndCap <- Drawing2D.LineCap.ArrowAnchor
        this.UpdatePositionManiglie(PointF(this.Position.X, this.Position.Y)) 
        
        match image with
            | Some img -> 
                use tBrush = new TextureBrush(img, Drawing2D.WrapMode.Clamp)
                let m11 = this.Width / single img.Width
                let m22 = this.Height / single img.Height
                let x = position.X
                let y = position.Y
                tBrush.Transform <- new Drawing2D.Matrix(m11, 0.0f, 0.0f, m22, x, y);
                g.FillEllipse(tBrush, r);

            | _ -> ()
        
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.DrawEllipse(new Pen(Color.Blue,penWidth), r)
        g.DrawLine(vectorP, this.Position, PointF(this.Position.X + this.VectorEnd.X, this.Position.Y + this.VectorEnd.Y))

    override this.HitTest p =
        let x = this.Position.X - p.X
        let y = this.Position.Y - p.Y
        let asseMax = max (this.Width/2.f) (this.Height/2.f)
        let asseMin = min (this.Width/2.f) (this.Height/2.f)
        (x*x) / (asseMax*asseMax) + (y*y)/(asseMin*asseMin) < 1.f 


type MButton =
    | ZoomIn = 4
    | ZoomOut = 5
    | Up = 6
    | Down = 7
    | Right = 8
    | Left = 9
    | Rotate = 10
    | AntiRotate = 11


type Container() as this =
    inherit LWCContainer()

    let id = 0

    let buttons = [|MyButton(Position=PointF(2.f, 150.f),Size=SizeF(80.f, 40.f),Text="Add Planet"); 
                    MyButton(Position=PointF(2.f, 200.f),Size=SizeF(80.f, 40.f),Text="Delete Planet");
                    MyButton(Position=PointF(2.f, 250.f),Size=SizeF(80.f, 40.f),Text="Add Image"); 
                    MyButton(Position=PointF(2.f, 300.f),Size=SizeF(80.f, 40.f),Text="Animation");
                    MyButton(Position=PointF(150.f, 2.f),Size=SizeF(30.f, 30.f),Text="+");
                    MyButton(Position=PointF(200.f, 2.f),Size=SizeF(30.f, 30.f),Text="-");
                    MyButton(Position=PointF(250.f, 2.f),Size=SizeF(30.f, 30.f),Text="↑");
                    MyButton(Position=PointF(300.f, 2.f),Size=SizeF(30.f, 30.f),Text="↓");
                    MyButton(Position=PointF(350.f, 2.f),Size=SizeF(30.f, 30.f),Text="→");
                    MyButton(Position=PointF(400.f, 2.f),Size=SizeF(30.f, 30.f),Text="←");
                    MyButton(Position=PointF(450.f, 2.f),Size=SizeF(30.f, 30.f),Text="↷");
                    MyButton(Position=PointF(500.f, 2.f),Size=SizeF(30.f, 30.f),Text="↶");|]
    
    let planets = ResizeArray<Planet>()
    let mutable creatingPlanets = false
    let mutable dragging: (Planet) option = None
    let mutable draggingManiglia: (int * Planet) option = None
    let mutable offsetDrag = PointF()
    let mutable selectedPlanet: Planet option = None
    let mutable selectedManiglia: Maniglia option = None
    let mutable boolClickManiglia = false //booleano che indica se c'è una maniglia selezionata

    let setCreatingMode() =
        creatingPlanets <- true 
    
    let animationTimer = new Timer(Interval = 1000/60)

    let addPlanet(p: PointF) =
        let position = this.WV.TransformPointV(p)
        let newPlanet = Planet(Position=position, Size=SizeF(80.f, 80.f), CoordinateType = World)
        newPlanet.Mass <- (float32) System.Math.PI * newPlanet.Width/2.f * newPlanet.Height/2.f

        let upManiglia = Maniglia(Position=PointF(position.X, position.Y - newPlanet.Height/2.f), Size=SizeF(15.f, 15.f), CoordinateType = World)
        let downManiglia = Maniglia(Position=PointF(position.X, newPlanet.Position.Y + newPlanet.Height/2.f), Size=SizeF(15.f, 15.f), CoordinateType = World)
        let leftManiglia = Maniglia(Position=PointF(position.X - newPlanet.Width/2.f, position.Y), Size=SizeF(15.f, 15.f), CoordinateType = World)
        let rightManiglia = Maniglia(Position=PointF(position.X + newPlanet.Width/2.f, position.Y), Size=SizeF(15.f, 15.f), CoordinateType = World)
        let vectorManiglia = Maniglia(Position=PointF(position.X + newPlanet.VectorEnd.X, position.Y + newPlanet.VectorEnd.Y), Size=SizeF(10.f, 10.f), CoordinateType = World)
        newPlanet.Maniglie.Add(upManiglia)
        newPlanet.Maniglie.Add(downManiglia)
        newPlanet.Maniglie.Add(leftManiglia)
        newPlanet.Maniglie.Add(rightManiglia)
        newPlanet.Maniglie.Add(vectorManiglia)

        planets.Add(newPlanet)
        newPlanet.Parent <- Some(this :> UserControl)
        this.Invalidate()

    let deletePlanet() =
        match selectedPlanet with
            | Some planet -> 
                planets.Remove(planet) |> ignore
                selectedPlanet <- None

            | _ -> ()
        this.Invalidate()

    let addImage() =
        match selectedPlanet with
        | Some planet -> 
            use dialog = new OpenFileDialog()
            dialog.Title <- "Select an Image"
            dialog.Filter <- "images| *.JPG; *.PNG; *.GIF"
            if(dialog.ShowDialog() = DialogResult.OK) then
                let image = new Bitmap(dialog.FileName)
                planet.AddImage(Some image) |> ignore
                this.Invalidate()
        | _ -> ()

    let animate() = 
        if (animationTimer.Enabled) then
            animationTimer.Stop()
        else 
            animationTimer.Start()

    let movingPlanets control = 
        match control with 
            | MButton.Up -> this.WV.TranslateV(0.f, 10.f)
            | MButton.Down -> this.WV.TranslateV(0.f, -10.f)
            | MButton.Left -> this.WV.TranslateV(10.f, 0.f)
            | MButton.Right -> this.WV.TranslateV(-10.f, 0.f)
            | MButton.Rotate ->
                this.WV.TranslateV(this.Width/2 |> single, this.Height/2 |> single)
                this.WV.RotateV(-10.f)
                this.WV.TranslateV(-this.Width/2 |> single, -this.Height/2 |> single)
            | MButton.AntiRotate ->
                this.WV.TranslateV(this.Width/2 |> single, this.Height/2 |> single)
                this.WV.RotateV(10.f)
                this.WV.TranslateV(-this.Width/2 |> single, -this.Height/2 |> single)
            | MButton.ZoomIn ->
                let cx, cy = this.Width/2 |> single, this.Height/2 |> single
                let p = PointF(cx, cy) |> this.WV.TransformPointV
                this.WV.ScaleV(1.f/1.1f, 1.f/1.1f)
                let pScaled = PointF(cx, cy)  |> this.WV.TransformPointV
                this.WV.TranslateW(pScaled.X - p.X, pScaled.Y - p.Y)
            | MButton.ZoomOut ->
                let cx, cy = this.Width/2 |> single, this.Height/2 |> single
                let p = PointF(cx, cy) |> this.WV.TransformPointV
                this.WV.ScaleV(1.1f, 1.1f)
                let pScaled = PointF(cx, cy)  |> this.WV.TransformPointV
                this.WV.TranslateW(pScaled.X - p.X, pScaled.Y - p.Y)
            | _ -> ()
        this.Invalidate()

    let movingTimer = new Timer(Interval = 20)
    let mutable moveControl = MButton.Up
    let buttonsList = [MButton.ZoomIn; MButton.ZoomOut; MButton.Up; MButton.Down; MButton.Right; MButton.Left; MButton.Rotate; MButton.AntiRotate]

    do
        movingTimer.Tick.Add(fun _ -> movingPlanets moveControl)
        for c in buttonsList do 
            buttons.[int (c)].MouseDown.Add(fun _ -> moveControl <- c)
            buttons.[int (c)].MouseDown.Add(fun _ -> movingTimer.Start())

        buttons.[0].MouseUp.Add(fun _ -> setCreatingMode())
        buttons.[1].MouseUp.Add(fun _ -> deletePlanet())
        buttons.[2].MouseUp.Add(fun _ -> addImage())
        buttons.[3].MouseUp.Add(fun _ -> animate())
        for butt in buttons do
            this.LWControls.Add(butt)

    do
        animationTimer.Tick.Add(fun _ ->
            let cost = 0.02f // simula il tempo
            let costGrav = 10.f
            for p1 in planets do 
                for p2 in planets do    
                    if(not (p1.Equals(p2))) then
                        let distanza = PointF(p2.Position.X - p1.Position.X, p2.Position.Y - p1.Position.Y)
                        let mutable distQuad = distanza.X*distanza.X + distanza.Y*distanza.Y
                        if(distQuad < 0.1f) then
                            distQuad <- 0.1f
                        let vettoreUnitario = PointF(distanza.X/sqrt(distQuad), distanza.Y/sqrt(distQuad))
                        let acc = PointF(vettoreUnitario.X*(costGrav*p2.Mass/distQuad), vettoreUnitario.Y*(costGrav*p2.Mass/distQuad))
                        p1.VectorEnd <- PointF(p1.VectorEnd.X + acc.X, p1.VectorEnd.Y + acc.Y)

            for planet in planets do
                planet.Position <- PointF(planet.Position.X + planet.VectorEnd.X*cost, planet.Position.Y + planet.VectorEnd.Y*cost)
            this.Invalidate()
        )

    override this.OnMouseDown e =
        let position = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest position)) with
            | Some control -> 
                    if(creatingPlanets) then
                        creatingPlanets <- false
                    control.OnMouseDown(e)
            | _ -> 
                match creatingPlanets with
                    | true -> addPlanet(position)
                    | _ ->
                        let pw = this.WV.TransformPointV position

                        for planet in planets  do
                            match (planet.Maniglie |> Seq.tryFindIndexBack (fun m -> m.HitTest pw)) with
                                | Some maniglia -> 
                                    // ho cliccato su una maniglia
                                    let m = planet.Maniglie.[maniglia]
                                    let mutable from = m.Position
                                    match selectedManiglia with
                                        | Some m -> m.PenWidth <- 1.f
                                        | None -> ()
                                    boolClickManiglia <- true
                                    m.PenWidth <- 2.f
                                    selectedManiglia <- Some (m)
                                    draggingManiglia <- Some (maniglia, planet)
                                    offsetDrag <- PointF(m.Position.X - pw.X, m.Position.Y - pw.Y)
                                | _ -> ()

                        match (planets |> Seq.tryFindBack (fun c -> c.HitTest pw)) with
                            | Some control -> 
                                // ho cliccato su un pianeta
                                let mutable from = control.Position
                                if (not boolClickManiglia) then 
                                    match selectedPlanet with
                                        | Some planet -> 
                                                planet.PenWidth <- 1.f
                                        | None -> ()
                                    control.PenWidth <- 2.f
                                    selectedPlanet <- Some control

                                    match selectedManiglia with
                                        | Some maniglia -> maniglia.PenWidth <- 1.f
                                        | None -> ()
                                    selectedManiglia <- None

                                if (boolClickManiglia) then 
                                    match selectedPlanet with
                                        | Some planet -> 
                                                planet.PenWidth <- 1.f
                                        | None -> ()
                                    selectedPlanet <- None

                                if (draggingManiglia.IsNone) then
                                    dragging <- Some (control)
                                    offsetDrag <- PointF(control.Position.X - pw.X, control.Position.Y - pw.Y)
                               
                            | _ ->
                                // non hai cliccato su un pianeta
                                match selectedPlanet with
                                    | Some planet -> 
                                        planet.PenWidth <- 1.f
                                    | None -> ()
                                if (not boolClickManiglia) then
                                    match selectedManiglia with
                                        | Some maniglia -> maniglia.PenWidth <- 1.f
                                        | None -> ()
                                    selectedManiglia <- None
                                selectedPlanet <- None
                               
                        boolClickManiglia <- false      
                        this.Invalidate()


    override this.OnMouseUp e =
        let position = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest position)) with
            | Some control -> control.OnMouseUp(e)
            | _ -> ()       
        dragging <- None
        draggingManiglia <- None
        movingTimer.Stop()


    override this.OnMouseMove e =
        let position = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        let controlDragging() = 
            let pw = this.WV.TransformPointV position
            match draggingManiglia with
                | Some (m, planet) -> 
                   let maniglia = planet.Maniglie.[m]
                   match m with 
                        | 0 -> 
                            let diff = maniglia.Position.Y - (pw.Y + offsetDrag.Y)
                            planet.Size <- SizeF(planet.Width, planet.Height + 2.f*diff)
                        | 1 -> 
                            let diff = -maniglia.Position.Y + pw.Y + offsetDrag.Y
                            planet.Size <- SizeF(planet.Width, planet.Height + 2.f*diff)
                        | 2 ->
                            let diff = maniglia.Position.X - (pw.X + offsetDrag.X)
                            planet.Size <- SizeF(planet.Width + 2.f*diff, planet.Height)
                        | 3 -> 
                            let diff = -maniglia.Position.X + pw.X + offsetDrag.X
                            planet.Size <- SizeF(planet.Width + 2.f*diff, planet.Height)
                        | 4 -> 
                            let x = pw.X + offsetDrag.X - planet.Position.X
                            let y = pw.Y + offsetDrag.Y - planet.Position.Y
                            planet.VectorEnd <- PointF(x, y)
                        |_ -> ()

                   this.Invalidate()
                | _ -> 
                    match dragging with
                        | Some (planet) -> 
                           planet.Position <- PointF(pw.X + offsetDrag.X, pw.Y + offsetDrag.Y)
                           this.Invalidate()
                        | _ -> ()

        match (controlsView |> Seq.tryFind(fun c -> c.HitTest position)) with
            | Some control -> 
               control.OnMouseMove(e)
               controlDragging()
            | None -> controlDragging()
        

    override this.OnPaint e =
        let g = e.Graphics
        let t = g.Transform
        g.Transform <- this.WV.WV
        planets |> Seq.iter (fun b -> b.OnPaint e)
        for planet in planets do
            planet.Maniglie |> Seq.iter (fun b -> b.OnPaint e)
        g.Transform <- t
        for idx in (this.LWControls.Count - 1) .. -1 .. 0 do
            let c = this.LWControls.[idx]
            if c.CoordinateType = View then
                c.OnPaint e    


let lwcc = new Container(Dock=DockStyle.Fill)
let f = new Form(Text="MidTerm", TopMost = true)
f.Size <- Size(700,700)
f.Controls.Add(lwcc)
f.Show()