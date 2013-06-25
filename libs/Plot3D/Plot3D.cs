using System;
using System.Windows.Forms;

namespace Plot3D {

    public partial class Plot3D : UserControl {
        
        private double obsX;
        private double obsY;
        private double obsZ;
        private int xs0;
        private int ys0;
        private double screenDistance;
        private double screenWidthPhys;
        private double screenHeightPhys;
        private Surface3DRenderer sr;

        /// <param name="obsX">Observator's X position</param>
        /// <param name="obsY">Observator's Y position</param>
        /// <param name="obsZ">Observator's Z position</param>
        /// <param name="xs0">X coordinate of screen</param>
        /// <param name="ys0">Y coordinate of screen</param>
        /// <param name="screenDistance">The screen distance.</param>
        /// <param name="screenWidthPhys">Width of the screen in meters.</param>
        /// <param name="screenHeightPhys">Height of the screen in meters.</param>
        public Plot3D(Func<double, double, double> function, double obsX = 70, double obsY = 35, double obsZ = 40, int xs0 = 0, int ys0 = 0, double screenDistance = 0.5, double screenWidthPhys = 0, double screenHeightPhys = 0) {
            this.obsX = obsX;
            this.obsY = obsY;
            this.obsZ = obsZ;
            this.xs0 = xs0;
            this.ys0 = ys0;
            this.screenDistance = screenDistance;
            this.screenWidthPhys = screenWidthPhys;
            this.screenHeightPhys = screenHeightPhys;
            this.sr = new Surface3DRenderer(obsX, obsY, obsZ, xs0, ys0, ClientRectangle.Width, ClientRectangle.Height, screenDistance, screenWidthPhys, screenHeightPhys);
            this.sr.Function = function;
            OnResize(null);
            ResizeRedraw = true;
            DoubleBuffered = true;
        }

        public ColorSchema ColorSchema {
            get { return sr.ColorSchema; }
            set { sr.ColorSchema = value; }
        }

        public Func<double, double, double> Function {
            get { return sr.Function; }
            set { sr.Function = value; }
        }

        public void SetFunction(string formula) {
            sr.SetFunction(formula);
        }

        protected override void OnPaint(PaintEventArgs e) {
            base.OnPaint(e);
            e.Graphics.Clear(BackColor);
            sr.RenderSurface(e.Graphics);
        }

        protected override void OnResize(EventArgs e) {
            base.OnResize(e);
            sr.ReCalculateTransformationsCoeficients(obsX, obsY, obsZ, xs0, ys0, ClientRectangle.Width, ClientRectangle.Height, screenDistance, screenWidthPhys, screenHeightPhys);
        }
    }
}