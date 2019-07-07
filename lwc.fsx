#load "MidTerm.fs"
open MidTerm
open System.Windows.Forms
open System.Drawing

// Libreria

type WVMatrix () =
    let wv = new Drawing2D.Matrix()
    let vw = new Drawing2D.Matrix()

    member this.TranslateW (tx, ty) =
        wv.Translate(tx, ty)
        vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

    member this.TranslateV (tx, ty) =
        vw.Translate(tx, ty)
        wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)    

    member this.ScaleW (sx, sy) =
        wv.Scale(sx, sy)
        vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

    member this.ScaleV (sx, sy) =
        vw.Scale(sx, sy)
        wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

    member this.RotateW (a) =
        wv.Rotate(a)
        vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

    member this.RotateV (a) =
        vw.Rotate(a)
        wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

    member this.TransformPointW (p:PointF) =
        let a = [| p |]
        wv.TransformPoints(a)
        a.[0]

    member this.TransformPointV (p:PointF) =
        let a = [| p |]
        vw.TransformPoints(a)
        a.[0]

    member this.WV with get() = wv
    member this.VW with get() = vw
   
type CoordinateType = View | World
 
 //Utility Functions
let Rect2RectF (r:Rectangle) =
    RectangleF(single r.X, single r.Y, single r.Width, single r.Height)

let RectF2Rect (r:RectangleF) =
    Rectangle(int r.X, int r.Y, int r.Width, int r.Height)

let Point2PoitF (p : Point) =
    PointF(single p.X, single p.Y)

let PointF2Poit (p : PointF) =
    Point(int p.X, int p.Y)

        
type LWCControl() =
    let mutable coordinates = View
    let mutable sz = SizeF()
    let mutable pos = PointF()
    let mutable parent : UserControl option = None //Control = null

    let mousedownevt = new Event<MouseEventArgs>()
    let mousemoveevt = new Event<MouseEventArgs>()
    let mouseupevt = new Event<MouseEventArgs>()
    //add event
    member this.MouseDown = mousedownevt.Publish
    member this.MouseMove = mousemoveevt.Publish
    member this.MouseUp = mouseupevt.Publish
    //run function
    abstract OnMouseDown : MouseEventArgs -> unit
    default this.OnMouseDown e = mousedownevt.Trigger(e)

    abstract OnMouseMove : MouseEventArgs -> unit
    default this.OnMouseMove e = mousemoveevt.Trigger(e)

    abstract OnMouseUp : MouseEventArgs -> unit
    default this.OnMouseUp e = mouseupevt.Trigger(e)

    abstract OnPaint : PaintEventArgs -> unit
    default this.OnPaint e = ()

    member this.Invalidate() = 
        match parent with
        |   Some p -> p.Invalidate()
        |   None -> ()

    member this.CoordinateType
        with get() = coordinates
        and set(v) = coordinates <- v

    member this.Size
        with get() = sz
        and set(v) = sz <- v

    member this.Position
        with get() = pos
        and set(v) = pos <- v

    member this.Parent
        with get() = parent
        and set(v) = parent <- v

    member this.PositionInt with get() = Point(int pos.X, int pos.Y)
    
    member this.ClientSizeInt with get() = Size(int sz.Width, int sz.Height)

    member this.Left = pos.X

    member this.Top = pos.Y

    member this.Width = sz.Width

    member this.Height = sz.Height

    abstract HitTest : PointF -> bool
    default this.HitTest p =
        (new RectangleF(pos, sz)).Contains(p)


type LWCContainer() as this =
    inherit UserControl()

    let wv = WVMatrix()
    //genera evento
    let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()

    do  //double buffering
        this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
        controls.CollectionChanged.Add(fun e ->
        for i in e.NewItems do
            //? si controlla a runtime(downcast), qui sotto invece upcast
            (i :?> LWCControl).Parent <- Some(this :> UserControl)
    )

    member this.LWControls with get() = controls
    
    member this.WV with get() = wv

    override this.OnMouseDown e =
        this.MouseEventFunction e 0

    override this.OnMouseMove e =
        this.MouseEventFunction e 1

    override this.OnMouseUp e =
        this.MouseEventFunction e 2

    member this.MouseEventFunction e t  =
        let p = PointF(single e.X, single e.Y)
        let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
            |   Some c -> 
                    if (t = 0) then c.OnMouseDown(e)
                    elif(t = 1) then c.OnMouseMove(e)
                    else    c.OnMouseUp(e)
            |   None -> 
                    let pw = wv.TransformPointV p
                    let controlsWorld = controls |> Seq.filter (fun c -> c.CoordinateType = World)
                    match (controlsWorld |> Seq.tryFindBack(fun c -> c.HitTest pw)) with
                    |   Some c ->
                            if (t = 0) then c.OnMouseDown(e)
                            elif(t = 1) then c.OnMouseMove(e)
                            else    c.OnMouseUp(e)
                    |   None -> ()

    override this.OnKeyDown e =
        match e.KeyCode with
        |   Keys.W ->
                wv.TranslateV(0.f, 10.f)
        |   Keys.A -> 
                wv.TranslateV(10.f, 0.f)
        |   Keys.S -> 
                wv.TranslateV(0.f, -10.f)
        |   Keys.D ->
                wv.TranslateV(-10.f, 0.f)
        |   Keys.Q ->
                wv.TranslateV(this.Width / 2 |> single, this.Height / 2 |> single)
                wv.RotateV(10.f)
                wv.TranslateV(-this.Width / 2 |> single, -this.Height / 2 |> single)
        |   Keys.E ->
                wv.TranslateV(this.Width / 2 |> single, this.Height / 2 |> single)
                wv.RotateV(-10.f)
                wv.TranslateV(-this.Width / 2 |> single, -this.Height / 2 |> single)
        |   Keys.Z ->
                let cx, cy = this.Width / 2 |> single, this.Height / 2 |> single
                let po = PointF(cx, cy) |> wv.TransformPointV
                wv.ScaleV(1.1f, 1.1f)
                let pn = PointF(cx, cy) |> wv.TransformPointV
                wv.TranslateW(pn.X - po.X, pn.Y - po.Y)
        |   Keys.X ->
                let cx, cy = this.Width / 2 |> single, this.Height / 2 |> single
                let po = PointF(cx, cy) |> wv.TransformPointV
                wv.ScaleV(1.f/1.1f, 1.f/1.1f)
                let pn = PointF(cx, cy) |> wv.TransformPointV
                wv.TranslateW(pn.X - po.X, pn.Y - po.Y)
        | _ -> ()
        this.Invalidate()


