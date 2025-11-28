using System;
using System.Drawing;
using System.Windows.Forms;

namespace Pedidos
{
    public partial class FormPedido : Form
    {
        ToolStripButton btnPedidos;
        ToolStripButton btnInventario;
        ToolStripButton btnRecetas;
        ToolStripButton btnProduccion;
        ToolStripButton btnProductos;

        public FormPedido()
        {
            InitializeComponent();

            btnPedidos = new ToolStripButton("Pedidos");
            btnInventario = new ToolStripButton("Inventario");
            btnRecetas = new ToolStripButton("Recetas");
            btnProduccion = new ToolStripButton("Producción");
            btnProductos = new ToolStripButton("Productos");

            btnPedidos.Click += (s, e) => AbrirFormulario(new FormPedido(), btnPedidos);
            btnInventario.Click += (s, e) => AbrirFormulario(new FormInventario(), btnInventario);
            btnRecetas.Click += (s, e) => AbrirFormulario(new FormReceta(), btnRecetas);
            btnProduccion.Click += (s, e) => AbrirFormulario(new FormProduccion(), btnProduccion);
            btnProductos.Click += (s, e) => AbrirFormulario(new FormProducto(), btnProductos);

            tsP.Items.Add(btnPedidos);
            tsP.Items.Add(btnInventario);
            tsP.Items.Add(btnRecetas);
            tsP.Items.Add(btnProduccion);
            tsP.Items.Add(btnProductos);
        }

        private void AbrirFormulario(Form destino, ToolStripButton botonActivo)
        {
            ResetBotonEstilos();
            botonActivo.BackColor = ColorTranslator.FromHtml("#d96704");
            botonActivo.ForeColor = Color.White;
            destino.Show();
        }

        private void ResetBotonEstilos()
        {
            foreach (ToolStripItem item in tsP.Items)
            {
                if (item is ToolStripButton btn)
                {
                    btn.BackColor = Color.White;
                    btn.ForeColor = SystemColors.ControlText;
                }
            }
        }
    }
}
