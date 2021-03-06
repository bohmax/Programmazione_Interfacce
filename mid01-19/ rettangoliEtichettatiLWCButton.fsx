#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)
    
type RettEtichetta() =
    let mutable nome = ""
    let mutable font = new Font("Arial", 12.f)
    let mutable vuoto = false //l'elemento non è vuoto
    let mutable rett = new Rectangle(0,0,10,10)
    let mutable seleziona = false

    member this.Nomina
     with get() = nome
     and set(v) = nome <- v

    member this.AggFont
     with get() = font
     and set(v) = font <- v

    member this.Vuoto
      with get() = vuoto
      and set(v) = vuoto <- v

    member this.Rett
      with get() = rett
      and set(v) = rett <- v
 
    member this.Seleziona
        with get() = seleziona
        and set(v) = seleziona <- v

type ArchEtichetta() =
    let mutable nome = ""
    let mutable font = new Font("Arial", 12.f, FontStyle.Bold)
    let mutable vuoto = false //l'elemento non è vuoto
    let mutable nodo1 = new RettEtichetta()
    let mutable nodo2 = new RettEtichetta()
    let mutable seleziona = false

    member this.Nomina
      with get() = nome
      and set(v) = nome <- v
    
    member this.AggFont
     with get() = font
     and set(v) = font <- v

    member this.Vuoto
      with get() = vuoto
      and set(v) = vuoto <- v
    
    member this.Nodo1 
     with get() = nodo1
     and set(v) = nodo1 <- v
    
    member this.Nodo2 
     with get() = nodo2
     and set(v) = nodo2 <- v

    member this.Points = //punti di disegno dell'arco
        let jLoc = nodo1.Rett.Location
        let jRett = nodo1.Rett
        let iLoc = nodo2.Rett.Location
        let iRett = nodo2.Rett
        let mutable diff = 0
        if iLoc.Y>jLoc.Y then
            diff <- iLoc.Y - jLoc.Y
        else
            diff <- jLoc.Y - iLoc.Y
        if diff>=0 && diff<=iRett.Height then
            if iLoc.X > jLoc.X then //j sta a sx
                PointF(float32(iLoc.X),float32(iLoc.Y+iRett.Height/2)),PointF(float32(jLoc.X+jRett.Width),float32(jLoc.Y+iRett.Height/2))
            else
                PointF(float32(iLoc.X+iRett.Width),float32(iLoc.Y+iRett.Height/2)),PointF(float32(jLoc.X),float32(jLoc.Y+iRett.Height/2))
         else
            if iLoc.Y < jLoc.Y then //j sta sotto
                PointF(float32(iLoc.X+iRett.Width/2),float32(iLoc.Y+iRett.Height)),PointF(float32(jLoc.X+jRett.Width/2),float32(jLoc.Y))
            else //sta sopra
                PointF(float32(iLoc.X+iRett.Width/2),float32(iLoc.Y)),PointF(float32(jLoc.X+jRett.Width/2),float32(jLoc.Y+jRett.Height))

    member this.Seleziona
        with get() = seleziona
        and set(v) = seleziona <- v

let checkPickCorrelationRett (rett:Rectangle) (p:Point) = //pick correlation di un punto su un rettangolo
    if rett.Contains p then 
        true
    else false

let rec trovaRett (l:RettEtichetta list) (p:Point) =  //trova se stiamo cliccando in uno dei rettangoli della lista
    match l with
    | [] -> new RettEtichetta(Vuoto=true)
    | head :: tail -> 
    //if not ( this.checkPickCorrelationRett head.Rett p ) then //se usa quella dei LWC
    if not ( checkPickCorrelationRett head.Rett p ) then 
        trovaRett tail p
    else head

let checkPickCorrelationArch (arco:ArchEtichetta) (p:Point) = //pick correlation di un punto su un arco
    let (punto1, punto2) = arco.Points
    let minX = int( min punto1.X punto2.X)
    let maxX = int( max punto1.X punto2.X)
    let minY = int( min punto1.Y punto2.Y)
    let maxY = int( max punto1.Y punto2.Y)
    if p.X>=minX && p.X<=maxX && p.Y>=minY && p.Y<= maxY then
        let mutable esitoX = 0.f
        let mutable esitoY = 0.f
        let mutable diff = 0.f
        //printfn "%A %A %A" punto1 punto2 p
        if (punto2.X - punto1.X) = 0.f then
            diff <- (float32(p.X) - punto1.X)
        else if (punto2.Y - punto1.Y) = 0.f then
            diff <- (float32(p.Y) - punto1.Y)
        else
            esitoX <- float32((float32(p.X) - punto1.X)/(punto2.X - punto1.X))
            esitoY <- float32((float32(p.Y) - punto1.Y)/(punto2.Y - punto1.Y))
            diff <- esitoX - esitoY
        if abs(diff) < 0.05f then 
            true
        else false
    else false

let rec trovaArch (l:ArchEtichetta list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> new ArchEtichetta(Vuoto=true)
    | head :: tail -> 
    if not ( checkPickCorrelationArch head e ) then 
        trovaArch tail e
    else head   

let checkDoubleArch (l:ArchEtichetta list) (elemento: ArchEtichetta) = //controllo se c'è un arco duplicato
    let mutable found:int = 0
    for i in l do
        if (i.Nodo1=elemento.Nodo1 && i.Nodo2=elemento.Nodo2) || (i.Nodo2=elemento.Nodo1 && i.Nodo1=elemento.Nodo2) then
            found <- 1
    found

let mutable listaRett = [] //lista oggetti RettEtichetta nell'Area
let mutable listaArch = [] //lista oggetti ArchEtichetta nell'Area
let mutable indRettSelected = new RettEtichetta(Vuoto=true) //elemento della listaRett da dover spostare/selezionare
let mutable indArchSelected = new ArchEtichetta(Vuoto=true) //elemento della listaArch da dover spostare/selezionare
let mutable LWCArea = new LWCControl()

let mutable contaRotate = 0 //perché il clipping funziona soltanto su coordinate normalizzate per cui per spostare a dx, sx su un oggetto obliquo 
    //la continua a spostare in obliquo e non a dx o sx

type Area() =
    inherit LWCControl()

    let mutable stillclick = false
    
    override this.OnMouseDown(e) =
        if listaRett.IsEmpty then //viene aggiunto RettEtichetta per la prima volta
            let rettNuovo = new RettEtichetta(Nomina="vuoto", Seleziona=true)
            rettNuovo.Rett <- new Rectangle(e.Location.X,e.Location.Y,int(rettNuovo.AggFont.Size)*rettNuovo.Nomina.Length,int(rettNuovo.AggFont.Height))
            listaRett <- List.append listaRett [ rettNuovo ] //l'= va solo la prima volta che viene aggiunto l'elemento
            indRettSelected <- rettNuovo 
            this.Invalidate()
        else
            let selectedRett = (trovaRett listaRett e.Location) //controllo se abbiamo selezionato un RettEtichetta 
            if selectedRett.Vuoto then 
                let selectedArch = (trovaArch listaArch e.Location) //controllo se abbiamo selezionato un ArchEtichetta
                if selectedArch.Vuoto then                
                    indArchSelected.Seleziona <- false
                    indArchSelected <- new ArchEtichetta(Vuoto=true) //garanzia ulteriore che lo abbiamo deselezionato
                    let rettNuovo = new RettEtichetta(Nomina="vuoto", Seleziona=true) //aggiungi un RettEtichetta perché ho selezionato area vuota/nessun RettEtichetta già presente
                    rettNuovo.Rett <- new Rectangle(e.Location.X,e.Location.Y,int(rettNuovo.AggFont.Size)*rettNuovo.Nomina.Length,int(rettNuovo.AggFont.Height))
                    listaRett <- List.append listaRett [ rettNuovo  ] 
                    indRettSelected <- rettNuovo
                else 
                    indRettSelected.Seleziona <- false
                    indRettSelected <- new RettEtichetta(Vuoto=true) //garanzia che selezioniamo al massimo un oggetto alla volta
                    indArchSelected <- selectedArch //seleziona un ArchEtichetta per la prima volta 
                    indArchSelected.Seleziona <- true //diventa quello che già esiste                 
            else if indRettSelected.Vuoto then //seleziona un RettEtichetta per la prima volta 
                indArchSelected.Seleziona <- false //deseleziono quello precedentemente selezionato
                indArchSelected <- new ArchEtichetta(Vuoto=true) //garanzia che selezioniamo al massimo un oggetto alla volta                 
                indRettSelected.Seleziona <- false
                indRettSelected <- selectedRett //diventa quello che già esiste
                indRettSelected.Seleziona <- true
            else if indRettSelected<>selectedRett then //se è già selezionato un RettEtichetta aggiungerne arco
                let elementoArco = new ArchEtichetta(Nomina="empty",Nodo1=indRettSelected, Nodo2=selectedRett)
                if (checkDoubleArch listaArch elementoArco ) = 0 then
                    listaArch <- List.append listaArch [ elementoArco ]
                indArchSelected.Seleziona <- false 
                indArchSelected <- new ArchEtichetta(Vuoto=true)
                indRettSelected.Seleziona <- false
                indRettSelected <- selectedRett
                indRettSelected.Seleziona <- true
            else if indRettSelected = selectedRett then   
                indRettSelected.Seleziona <- not(indRettSelected.Seleziona)
                if not(indRettSelected.Seleziona) then
                    indRettSelected <- new RettEtichetta(Vuoto=true)
            stillclick <- true           
            this.Invalidate()

    override this.OnMouseMove(e) =
        if not(indRettSelected.Vuoto) && stillclick then //muove RettEtichetta
            indRettSelected.Rett <- new Rectangle(e.Location.X, e.Location.Y, indRettSelected.Rett.Width, indRettSelected.Rett.Height) 
            this.Invalidate()

    override this.OnMouseUp(e) =
        //if stillclick then //se è stato mosso non interessa sapere che deve essere collegato
        //    indRettSelected <- new RettEtichetta(Vuoto=true)
        stillclick <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        LWCArea.WV.RotateW(float32 contaRotate)
        e.Graphics.Transform <- LWCArea.WV.WV
        for i in listaRett do 
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
        if indRettSelected.Seleziona then //disegno selezione RettEtichetta
            g.DrawRectangle(Pens.Black, new Rectangle(indRettSelected.Rett.X, indRettSelected.Rett.Y, indRettSelected.Rett.Width, indRettSelected.Rett.Height))
        for j in listaArch do
            let (point1, point2) = j.Points
            g.DrawLine(Pens.Black, point1, point2)
            let mezzoX = (point1.X+point2.X)/2.f
            let mezzoY = (point1.Y+point2.Y)/2.f
            g.DrawString(j.Nomina, j.AggFont, Brushes.Green, PointF(float32(mezzoX), float32(mezzoY)))
        if indArchSelected.Seleziona then //disegno selezione ArchEtichetta
            let (point1, point2) = indArchSelected.Points
            g.DrawLine(Pens.Blue, point1, point2)    
        LWCArea.WV.RotateW(float32 -contaRotate)
        e.Graphics.Transform <- LWCArea.WV.WV
    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))
        this.Invalidate()
        base.OnResize e

type DeleteButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) =
        if indRettSelected.Seleziona then
            let mutable newLista = []
            let mutable newArco = []
            for i in listaRett do
                if indRettSelected <> i then
                    newLista <- List.append newLista [i]
            for i in listaArch do
                if not(i.Nodo1 = indRettSelected || i.Nodo2 = indRettSelected) then
                    newArco <- List.append newArco [i]
            listaArch <- newArco
            listaRett <- newLista            
            indRettSelected <- new RettEtichetta(Vuoto=true) 
        if indArchSelected.Seleziona then
            let mutable newArco = []
            for i in listaArch do
                if not(i = indArchSelected) then
                    newArco <- List.append newArco [i]
            listaArch <- newArco
            indArchSelected <- new ArchEtichetta(Vuoto=true)
        this.Invalidate()

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AliceBlue, new Rectangle(0,0,50,50))
        g.DrawString("Delete", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

let textBox = new TextBox(Top=50, Left=0, Width=50)
type NameButton() =
    inherit LWCControl() 
    
    override this.OnMouseUp(e) =    
        if indRettSelected.Seleziona && textBox.Text.Length>0 then
            indRettSelected.Nomina <- textBox.Text                              //il seguente viene usato per i nomi lunghi
            indRettSelected.Rett <- new Rectangle(indRettSelected.Rett.X,indRettSelected.Rett.Y,int(indRettSelected.AggFont.Size)*indRettSelected.Nomina.Length,int(indRettSelected.AggFont.Height))
        if indArchSelected.Seleziona && textBox.Text.Length>0 then
            indArchSelected.Nomina <- textBox.Text
        this.Invalidate() 

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Aquamarine, new Rectangle(0,0,50,50))
        g.DrawString("Name", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ZommaAumentaButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(float32(LWCArea.ClientSize.Width/2.f), float32(LWCArea.ClientSize.Height/2.f))   //aggiusta le coordinate rispetto al centro della clientSize 
        LWCArea.WV.ScaleW(2.f,2.f)
        this.Invalidate()
        LWCArea.WV.TranslateW(-float32(LWCArea.ClientSize.Width/2.f), -float32(LWCArea.ClientSize.Height/2.f)) //aggiusta le coordinate rispetto al centro della clientSize 
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Yellow, new Rectangle(0,0,50,50))
        g.DrawString("Zomma+", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ZommaDecrementaButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(float32(LWCArea.ClientSize.Width/2.f), float32(LWCArea.ClientSize.Height/2.f))
        LWCArea.WV.ScaleW(float32(0.5),float32(0.5))
        this.Invalidate()
        LWCArea.WV.TranslateW(-float32(LWCArea.ClientSize.Width/2.f), -float32(LWCArea.ClientSize.Height/2.f))
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Orange, new Rectangle(0,0,50,50))
        g.DrawString("Zomma-", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type RotateDestraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        contaRotate <- (contaRotate+45)%360
        this.Invalidate() //notifica al SistemaGrafico che stai modificando qualcosa, 
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Blue, new Rectangle(0,0,50,50))
        g.DrawString("RotateDX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))


type RotateSinistraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        contaRotate <- (contaRotate-45)%360
        this.Invalidate()        
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Green, new Rectangle(0,0,50,50))
        g.DrawString("RotateSX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriDestraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(10.f, 0.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Bisque, new Rectangle(0,0,50,50))
        g.DrawString("ScorriDX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriSinistraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(-10.f, 0.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Chocolate, new Rectangle(0,0,50,50))
        g.DrawString("ScorriSX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriAltoButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(0.f, -10.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AliceBlue, new Rectangle(0,0,50,50))
        g.DrawString("ScorriSu", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriBassoButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(0.f, 10.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AntiqueWhite, new Rectangle(0,0,50,50))
        g.DrawString("ScorriGiù", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

 // -------------------------------- creazione container -------------------------------------------
let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Area(Position=PointF(50.f, 0.f),ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
LWCArea <- r
lwcc.LWControls.Add(r)
let d = new DeleteButton(Position=PointF(0.f, 0.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(d)
let n = new NameButton(Position=PointF(0.f, 50.f),ClientSize=SizeF(50.f , 50.f))
lwcc.LWControls.Add(n)
lwcc.Controls.Add(textBox)
let zA = new ZommaAumentaButton(Position=PointF(0.f,100.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(zA)
let zD = new ZommaDecrementaButton(Position=PointF(0.f,150.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(zD)
let rD = new RotateDestraButton(Position=PointF(0.f,200.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(rD)
let rS = new RotateSinistraButton(Position=PointF(0.f,250.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(rS)
let sD = new ScorriDestraButton(Position=PointF(0.f,300.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sD)
let sS = new ScorriSinistraButton(Position=PointF(0.f,350.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sS)
let sA = new ScorriAltoButton(Position=PointF(0.f,400.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sA)
let sB = new ScorriBassoButton(Position=PointF(0.f,450.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sB)

//----------------------------------------------------------------------------------------------
f.Controls.Add(lwcc)
f.Show()
