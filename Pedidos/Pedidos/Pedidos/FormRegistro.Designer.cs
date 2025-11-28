namespace Pedidos
{
    partial class FormRegistro
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormRegistro));
            this.gbLogin = new System.Windows.Forms.GroupBox();
            this.txtPassReg = new System.Windows.Forms.TextBox();
            this.lblPassReg = new System.Windows.Forms.Label();
            this.lblReg = new System.Windows.Forms.Label();
            this.btnRegistro = new System.Windows.Forms.Button();
            this.txtCorreoReg = new System.Windows.Forms.TextBox();
            this.lblCorreoReg = new System.Windows.Forms.Label();
            this.txtNombre = new System.Windows.Forms.TextBox();
            this.lblNombre = new System.Windows.Forms.Label();
            this.lblRegistro = new System.Windows.Forms.Label();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.lblDummy = new System.Windows.Forms.Label();
            this.gbLogin.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // gbLogin
            // 
            this.gbLogin.BackColor = System.Drawing.Color.White;
            this.gbLogin.Controls.Add(this.txtPassReg);
            this.gbLogin.Controls.Add(this.lblPassReg);
            this.gbLogin.Controls.Add(this.lblReg);
            this.gbLogin.Controls.Add(this.btnRegistro);
            this.gbLogin.Controls.Add(this.txtCorreoReg);
            this.gbLogin.Controls.Add(this.lblCorreoReg);
            this.gbLogin.Controls.Add(this.txtNombre);
            this.gbLogin.Controls.Add(this.lblNombre);
            this.gbLogin.Controls.Add(this.lblRegistro);
            this.gbLogin.Controls.Add(this.pictureBox1);
            this.gbLogin.Location = new System.Drawing.Point(375, 102);
            this.gbLogin.Name = "gbLogin";
            this.gbLogin.Size = new System.Drawing.Size(420, 520);
            this.gbLogin.TabIndex = 1;
            this.gbLogin.TabStop = false;
            // 
            // txtPassReg
            // 
            this.txtPassReg.Location = new System.Drawing.Point(28, 359);
            this.txtPassReg.Name = "txtPassReg";
            this.txtPassReg.Size = new System.Drawing.Size(371, 20);
            this.txtPassReg.TabIndex = 9;
            // 
            // lblPassReg
            // 
            this.lblPassReg.AutoSize = true;
            this.lblPassReg.Font = new System.Drawing.Font("MV Boli", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblPassReg.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.lblPassReg.Location = new System.Drawing.Point(24, 335);
            this.lblPassReg.Name = "lblPassReg";
            this.lblPassReg.Size = new System.Drawing.Size(92, 21);
            this.lblPassReg.TabIndex = 8;
            this.lblPassReg.Text = "Contraseña";
            // 
            // lblReg
            // 
            this.lblReg.AutoSize = true;
            this.lblReg.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.lblReg.Location = new System.Drawing.Point(136, 455);
            this.lblReg.Name = "lblReg";
            this.lblReg.Size = new System.Drawing.Size(160, 13);
            this.lblReg.TabIndex = 7;
            this.lblReg.Text = "¿Ya tienes cuenta? Inicia sesión";
            this.lblReg.Click += new System.EventHandler(this.lblReg_Click);
            // 
            // btnRegistro
            // 
            this.btnRegistro.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnRegistro.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnRegistro.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Italic))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnRegistro.ForeColor = System.Drawing.Color.White;
            this.btnRegistro.Location = new System.Drawing.Point(28, 402);
            this.btnRegistro.Name = "btnRegistro";
            this.btnRegistro.Size = new System.Drawing.Size(371, 36);
            this.btnRegistro.TabIndex = 6;
            this.btnRegistro.Text = "Registrarse";
            this.btnRegistro.UseVisualStyleBackColor = false;
            // 
            // txtCorreoReg
            // 
            this.txtCorreoReg.Location = new System.Drawing.Point(28, 289);
            this.txtCorreoReg.Name = "txtCorreoReg";
            this.txtCorreoReg.Size = new System.Drawing.Size(371, 20);
            this.txtCorreoReg.TabIndex = 5;
            // 
            // lblCorreoReg
            // 
            this.lblCorreoReg.AutoSize = true;
            this.lblCorreoReg.Font = new System.Drawing.Font("MV Boli", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblCorreoReg.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.lblCorreoReg.Location = new System.Drawing.Point(24, 265);
            this.lblCorreoReg.Name = "lblCorreoReg";
            this.lblCorreoReg.Size = new System.Drawing.Size(146, 21);
            this.lblCorreoReg.TabIndex = 4;
            this.lblCorreoReg.Text = "Correo Electrónico";
            // 
            // txtNombre
            // 
            this.txtNombre.Location = new System.Drawing.Point(28, 223);
            this.txtNombre.Name = "txtNombre";
            this.txtNombre.Size = new System.Drawing.Size(371, 20);
            this.txtNombre.TabIndex = 3;
            // 
            // lblNombre
            // 
            this.lblNombre.AutoSize = true;
            this.lblNombre.Font = new System.Drawing.Font("MV Boli", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblNombre.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.lblNombre.Location = new System.Drawing.Point(24, 199);
            this.lblNombre.Name = "lblNombre";
            this.lblNombre.Size = new System.Drawing.Size(143, 21);
            this.lblNombre.TabIndex = 2;
            this.lblNombre.Text = "Nombre completo";
            // 
            // lblRegistro
            // 
            this.lblRegistro.AutoSize = true;
            this.lblRegistro.Font = new System.Drawing.Font("MV Boli", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblRegistro.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.lblRegistro.Location = new System.Drawing.Point(129, 150);
            this.lblRegistro.Name = "lblRegistro";
            this.lblRegistro.Size = new System.Drawing.Size(157, 21);
            this.lblRegistro.TabIndex = 1;
            this.lblRegistro.Text = "Crear cuenta nueva";
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.Location = new System.Drawing.Point(157, 19);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(108, 109);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            // 
            // lblDummy
            // 
            this.lblDummy.AutoSize = true;
            this.lblDummy.Location = new System.Drawing.Point(32, 24);
            this.lblDummy.Name = "lblDummy";
            this.lblDummy.Size = new System.Drawing.Size(0, 13);
            this.lblDummy.TabIndex = 0;
            // 
            // FormRegistro
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(247)))), ((int)(((byte)(247)))), ((int)(((byte)(234)))));
            this.ClientSize = new System.Drawing.Size(1154, 685);
            this.Controls.Add(this.lblDummy);
            this.Controls.Add(this.gbLogin);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "FormRegistro";
            this.Text = "Family Bakery";
            this.Load += new System.EventHandler(this.Form2_Load);
            this.gbLogin.ResumeLayout(false);
            this.gbLogin.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox gbLogin;
        private System.Windows.Forms.Label lblReg;
        private System.Windows.Forms.Button btnRegistro;
        private System.Windows.Forms.TextBox txtCorreoReg;
        private System.Windows.Forms.Label lblCorreoReg;
        private System.Windows.Forms.TextBox txtNombre;
        private System.Windows.Forms.Label lblNombre;
        private System.Windows.Forms.Label lblRegistro;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.TextBox txtPassReg;
        private System.Windows.Forms.Label lblPassReg;
        private System.Windows.Forms.Label lblDummy;
    }
}