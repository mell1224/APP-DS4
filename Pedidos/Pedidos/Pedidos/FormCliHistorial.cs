using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Pedidos
{
    public partial class FormCliHistorial : Form
    {
        public FormCliHistorial()
        {
            InitializeComponent();
        }

        private void btnAtrasH_Click(object sender, EventArgs e)
        {
            FormCliente formCliente = new FormCliente();
            formCliente.Show();
            this.Hide();
        }
    }
}
