namespace global

open System.IO
open System.Windows
open System.Windows.Forms.Integration
open Plot3D

type SurfacePlotWindow(title, func, obsX, obsY, obsZ) as this =

    inherit Window()

    let chartHost = new WindowsFormsHost()

    do
        this.Width <- 600.0
        this.Height <- 600.0
        this.Content <- chartHost
        this.Title <- title
        let plot3D = new Plot3D(func, obsX, obsY, obsZ)
        chartHost.Child <- plot3D

    static member show title func (obsX, obsY, obsZ) = 
        let surfacePlotWindow = new SurfacePlotWindow(title, func, obsX, obsY, obsZ)
        surfacePlotWindow.Show()
        surfacePlotWindow.Activate() |> ignore
        surfacePlotWindow

    static member run title func (obsX, obsY, obsZ) = 
        let surfacePlotWindow = new SurfacePlotWindow(title, func, obsX, obsY, obsZ)
        surfacePlotWindow.ShowDialog() |> ignore