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

    let mutable id = 0
    let mutable penWidth = 1.f

    member this.ID
        with get() = id
        and set(v) = id <- v

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

    let mutable id = 0
    let wv =  WVMatrix()
    let mutable penWidth = 1.f
    let mutable image: Bitmap option = None
    let mutable mass = 0.f
    let mutable maniglie = ResizeArray<Maniglia>()

    member this.WV = wv

    member this.ID
        with get() = id
        and set(v) = id <- v

    member this.PenWidth
        with get() = penWidth
        and set(v) = penWidth <- v

    member this.AddImage img =
        image <- img
    
    member this.Mass
        with get() = mass
        and set(m) = mass <- m

    member  this.Maniglie with get() = maniglie      
        
    member this.UpdatePositionManiglie(position: PointF) =
        this.Maniglie.[0].Position <- PointF(position.X, position.Y - this.Height/2.f)
        this.Maniglie.[1].Position <- PointF(position.X, position.Y + this.Height/2.f)
        this.Maniglie.[2].Position <- PointF(position.X - this.Width/2.f, position.Y)
        this.Maniglie.[3].Position <- PointF(position.X + this.Width/2.f, position.Y)

    override this.OnPaint e =
        let g = e.Graphics
        let parent = this.Parent
        let position = PointF(this.Position.X - this.Width/2.f, this.Position.Y - this.Height/2.f)
        let r = RectF2Rect(RectangleF(position, this.Size)) 
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

    override this.HitTest p =
        let x = this.Position.X - p.X
        let y = this.Position.Y - p.Y
        let asseMax = max (this.Width/2.f) (this.Height/2.f)
        let asseMin = min (this.Width/2.f) (this.Height/2.f)
        (x*x)/(asseMax*asseMax) + (y*y)/(asseMin*asseMin) < 1.f


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

    let buttons = [|MyButton(Position=PointF(2.f, 150.f),Size=SizeF(60.f, 40.f),Text="Add Planet"); 
                    MyButton(Position=PointF(2.f, 200.f),Size=SizeF(60.f, 40.f),Text="Delete Planet");
                    MyButton(Position=PointF(2.f, 250.f),Size=SizeF(60.f, 40.f),Text="Add Image"); 
                    MyButton(Position=PointF(2.f, 300.f),Size=SizeF(60.f, 40.f),Text="Animation");
                    MyButton(Position=PointF(150.f, 2.f),Size=SizeF(30.f, 30.f),Text="+");
                    MyButton(Position=PointF(200.f, 2.f),Size=SizeF(30.f, 30.f),Text="-");
                    MyButton(Position=PointF(250.f, 2.f),Size=SizeF(30.f, 30.f),Text="↑");
                    MyButton(Position=PointF(300.f, 2.f),Size=SizeF(30.f, 30.f),Text="↓");
                    MyButton(Position=PointF(350.f, 2.f),Size=SizeF(30.f, 30.f),Text="→");
                    MyButton(Position=PointF(400.f, 2.f),Size=SizeF(30.f, 30.f),Text="←");
                    MyButton(Position=PointF(450.f, 2.f),Size=SizeF(30.f, 30.f),Text="↷");
                    MyButton(Position=PointF(500.f, 2.f),Size=SizeF(30.f, 30.f),Text="↶");|]
    
    let mutable id = 0
    let planets = ResizeArray<Planet>()
    let mutable creatingPlanets = false
    let mutable dragging = None
    let mutable draggingManiglia = None
    let mutable offsetDrag = PointF()
    let mutable selectedPlanet: Planet option = None
    let mutable selectedManiglia: Maniglia option = None

    let setCreatingMode() =
        creatingPlanets <- true 

    let addPlanet(p: PointF) =
        let position = this.WV.TransformPointV(p)
        id <- id + 1
        let newPlanet = Planet(Position=position, Size=SizeF(60.f, 60.f), ID=id, CoordinateType = World)
        newPlanet.Mass <- (float32) System.Math.PI * newPlanet.Width/2.f * newPlanet.Height/2.f

        let upManiglia = Maniglia(Position=PointF(position.X, position.Y - newPlanet.Height/2.f), Size=SizeF(10.f, 10.f), ID=id, CoordinateType = World)
        let downManiglia = Maniglia(Position=PointF(position.X, newPlanet.Position.Y + newPlanet.Height/2.f), Size=SizeF(10.f, 10.f), ID=id, CoordinateType = World)
        let leftManiglia = Maniglia(Position=PointF(position.X - newPlanet.Width/2.f, position.Y), Size=SizeF(10.f, 10.f), ID=id, CoordinateType = World)
        let rightManiglia = Maniglia(Position=PointF(position.X + newPlanet.Width/2.f, position.Y), Size=SizeF(10.f, 10.f), ID=id, CoordinateType = World)
        newPlanet.Maniglie.Add(upManiglia)
        newPlanet.Maniglie.Add(downManiglia)
        newPlanet.Maniglie.Add(leftManiglia)
        newPlanet.Maniglie.Add(rightManiglia)

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
        ()

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
                        match (planets |> Seq.tryFindBack (fun c -> c.HitTest pw)) with
                            | Some control -> 
                                // ho cliccato su un pianeta
                                let mutable from = control.Position
                                match selectedPlanet with
                                    | Some planet -> 
                                            planet.PenWidth <- 1.f
                                    | _ -> ()
                                match selectedManiglia with
                                        | Some maniglia -> 
                                                maniglia.PenWidth <- 1.f
                                                selectedManiglia <- None
                                        | _ -> ()
                                control.PenWidth <- 2.f
                                selectedPlanet <- Some control
                                dragging <- Some (control, from)
                                offsetDrag <- PointF(control.Position.X - pw.X, control.Position.Y - pw.Y)
                                control.OnMouseDown(e)
                            | _ ->
                                // non hai cliccato su un pianeta
                                match selectedPlanet with
                                    | Some planet -> 
                                        planet.PenWidth <- 1.f
                                    | _ -> ()
                                selectedPlanet <- None

                        for planet in planets  do
                            match (planet.Maniglie |> Seq.tryFindBack (fun c -> c.HitTest pw)) with
                                | Some control -> 
                                    // ho cliccato su una maniglia
                                    let mutable from = control.Position
                                    match selectedManiglia with
                                        | Some maniglia -> 
                                                maniglia.PenWidth <- 1.f
                                        | _ -> ()
                                    match selectedPlanet with
                                        | Some planet -> 
                                                planet.PenWidth <- 1.f
                                                selectedPlanet <- None
                                        | _ -> ()
                                    control.PenWidth <- 2.f
                                    selectedManiglia <- Some control
                                    draggingManiglia <- Some (control, from)
                                    offsetDrag <- PointF(control.Position.X - pw.X, control.Position.Y - pw.Y)
                                    control.OnMouseDown(e)
                                | _ ->
                                    // non hai cliccato su una maniglia
                                    match selectedManiglia with
                                        | Some maniglia -> 
                                            maniglia.PenWidth <- 1.f
                                        | _ -> ()
                                    selectedManiglia <- None
                                    selectedPlanet <- None

                        this.Invalidate()


    override this.OnMouseUp e =
        let position = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest position)) with
        | Some control -> control.OnMouseUp(e)
        | _ -> ()       
        dragging <- None
        // draggingManiglia <- None
        movingTimer.Stop()


    override this.OnMouseMove e =
        let position = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        let controlPlanet() = 
            let pw = this.WV.TransformPointV position
            match dragging with
                | Some (planet, position) -> 
                   planet.Position <- PointF(pw.X + offsetDrag.X, pw.Y + offsetDrag.Y)
                   planet.Invalidate()
                | _ -> ()
        //let controlManiglia() =
          //  let pw = this.WV.TransformPointV position
           // match draggingManiglia with
             //   | Some (maniglia, position) -> 
               //    maniglia.Position <- PointF(pw.X + offsetDrag.X, pw.Y + offsetDrag.Y)
                 //  maniglia.Invalidate()
               // | _ -> ()

        match (controlsView |> Seq.tryFind(fun c -> c.HitTest position)) with
            | Some control -> 
               control.OnMouseMove(e)
               controlPlanet()
               //controlManiglia()
            | None -> controlPlanet()
        



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