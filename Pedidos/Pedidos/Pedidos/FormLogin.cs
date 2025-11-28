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
    public partial class FormLogin : Form
    {
        public FormLogin()
        {
            InitializeComponent();
        }


        private void Form1_Load(object sender, EventArgs e)
        {
            ConfigurarPlaceholder(txtCorreo, "Ingresa tu correo", false);
            ConfigurarPlaceholder(txtPass, "Ingresa tu contraseña", true);
            FormCliente f3 = new FormCliente();
            f3.Show();
            FormPedido f4 = new FormPedido();
            f4.Show();
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

        private void lblLog_Click(object sender, EventArgs e)
        {
            FormRegistro f2 = new FormRegistro();
            f2.Show();
            this.Hide();
        }
    }
}
