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
    public partial class FormRegistro : Form
    {
        public FormRegistro()
        {
            InitializeComponent();
        }


       

        private void ConfigurarPlaceholder(TextBox txt, string placeholder, bool esPassword)
        {
            txt.Text = placeholder;
            txt.ForeColor = Color.Gray;
            txt.UseSystemPasswordChar = false; 

            txt.Enter += (s, ev) =>
            {
                if (txt.Text == placeholder)
                {
                    txt.Text = "";
                    txt.ForeColor = Color.Black;
                    if (esPassword) txt.UseSystemPasswordChar = true; 
                }
            };

            txt.Leave += (s, ev) =>
            {
                if (string.IsNullOrWhiteSpace(txt.Text))
                {
                    txt.Text = placeholder;
                    txt.ForeColor = Color.Gray;
                    if (esPassword) txt.UseSystemPasswordChar = false; 
                }
            };
        }

        private void lblReg_Click(object sender, EventArgs e)
        {
            FormLogin f1 = new FormLogin();
            f1.Show();
            this.Hide();
        }

        private void Form2_Load(object sender, EventArgs e)
        {

            ConfigurarPlaceholder(txtNombre, "Ingresa tu nombre completo", false);
            ConfigurarPlaceholder(txtCorreoReg, "Ingresa tu correo", false);
            ConfigurarPlaceholder(txtPassReg, "Ingresa tu contraseña", true);

            lblDummy.Focus();
        }

        
    }
}
