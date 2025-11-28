using System.Drawing;
using System.Windows.Forms;
using System;

namespace Pedidos
{
    public partial class FormCliente : Form
    {
        public FormCliente()
        {
            InitializeComponent();
            SetPlaceholder();
            txtBuscar.Enter += RemovePlaceholder;
            txtBuscar.Leave += SetPlaceholder;
            plTotal.Visible = false;

            btnPostres.Cursor = Cursors.Hand;
            btnSalados.Cursor = Cursors.Hand;
            btnBebidas.Cursor = Cursors.Hand;
            btnPanes.Cursor = Cursors.Hand;
            btnTodos.Cursor = Cursors.Hand;
            btnAgPF.Cursor = Cursors.Hand;
            btnAgCroiss.Cursor = Cursors.Hand;
            btnAgPI.Cursor = Cursors.Hand;
            btnAgBag.Cursor = Cursors.Hand;
            btnSalirCarrito.Cursor = Cursors.Hand;
            btnSalir.Cursor = Cursors.Hand;
            btnConfirmar.Cursor = Cursors.Hand;
            btnHistorial.Cursor = Cursors.Hand;
            pbCarrito.Cursor = Cursors.Hand;
        }

        private void FormCliente_Load(object sender, EventArgs e)
        {
            pbCarrito.Visible = false;
            plPConfir.Visible = false;
        }

        private void SetPlaceholder(object sender = null, EventArgs e = null)
        {
            if (string.IsNullOrWhiteSpace(txtBuscar.Text))
            {
                txtBuscar.Text = "Buscar productos...";
                txtBuscar.ForeColor = Color.Gray;
            }
        }

        private void RemovePlaceholder(object sender, EventArgs e)
        {
            if (txtBuscar.Text == "Buscar productos...")
            {
                txtBuscar.Text = "";
                txtBuscar.ForeColor = Color.Black;
            }
        }

        private void ResetButtonStyles()
        {
            btnPostres.BackColor = Color.White;
            btnPostres.ForeColor = SystemColors.ControlText;
            btnSalados.BackColor = Color.White;
            btnSalados.ForeColor = SystemColors.ControlText;
            btnBebidas.BackColor = Color.White;
            btnBebidas.ForeColor = SystemColors.ControlText;
            btnPanes.BackColor = Color.White;
            btnPanes.ForeColor = SystemColors.ControlText;
            btnTodos.BackColor = Color.White;
            btnTodos.ForeColor = SystemColors.ControlText;
        }

        private void btnPostres_Click(object sender, EventArgs e)
        {
            ResetButtonStyles();
            btnPostres.BackColor = ColorTranslator.FromHtml("#d96704");
            btnPostres.ForeColor = Color.White;

            plCroiss.Visible = false;
            plPF.Visible = false;
            plB.Visible = false;
            plPI.Visible = false;
        }

        private void btnSalados_Click(object sender, EventArgs e)
        {
            ResetButtonStyles();
            btnSalados.BackColor = ColorTranslator.FromHtml("#d96704");
            btnSalados.ForeColor = Color.White;

            plCroiss.Visible = false;
            plPF.Visible = false;
            plB.Visible = false;
            plPI.Visible = false;
        }

        private void btnBebidas_Click(object sender, EventArgs e)
        {
            ResetButtonStyles();
            btnBebidas.BackColor = ColorTranslator.FromHtml("#d96704");
            btnBebidas.ForeColor = Color.White;

            plCroiss.Visible = false;
            plPF.Visible = false;
            plB.Visible = false;
            plPI.Visible = false;
        }

        private void btnPanes_Click(object sender, EventArgs e)
        {
            ResetButtonStyles();
            btnPanes.BackColor = ColorTranslator.FromHtml("#d96704");
            btnPanes.ForeColor = Color.White;

            plCroiss.Visible = true;
            plPF.Visible = true;
            plB.Visible = true;
            plPI.Visible = true;
        }

        private void btnTodos_Click(object sender, EventArgs e)
        {
            ResetButtonStyles();
            btnTodos.BackColor = ColorTranslator.FromHtml("#d96704");
            btnTodos.ForeColor = Color.White;

            plCroiss.Visible = true;
            plPF.Visible = true;
            plB.Visible = true;
            plPI.Visible = true;
        }

        private void btnAgPF_Click(object sender, EventArgs e)
        {
            pbCarrito.Visible = true;
        }

        private void btnAgCroiss_Click(object sender, EventArgs e)
        {
            pbCarrito.Visible = true;
        }

        private void btnAgPI_Click(object sender, EventArgs e)
        {
            pbCarrito.Visible = true;
        }

        private void btnAgBag_Click(object sender, EventArgs e)
        {
            pbCarrito.Visible = true;
        }

        private void pbCarrito_Click(object sender, EventArgs e)
        {
            plTotal.Visible = true;
            plTotal.Focus();
        }

        private void btnSalirCarrito_Click(object sender, EventArgs e)
        {
            plTotal.Visible = false;
        }

        private void btnSalir_Click(object sender, EventArgs e)
        {
            this.Hide();
            FormLogin f1 = new FormLogin();
            f1.Show();
        }

        private void btnConfirmar_Click(object sender, EventArgs e)
        {
            plProducto1.Visible = false;
            lblDir.Visible = false;
            txtDir.Visible = false;
            lblObser.Visible = false;
            txtObservaciones.Visible = false;
            plMetodo.Visible = false;
            plPedidoTotal.Visible = false;
            plPConfir.Visible = true;
        }

        private void btnHistorial_Click(object sender, EventArgs e)
        {
            FormCliHistorial f4 = new FormCliHistorial();
            f4.Show();
            this.Hide();
        }
    }
}
