namespace Pedidos
{
    partial class FormReceta
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormReceta));
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnSalir = new System.Windows.Forms.Button();
            this.tsP = new System.Windows.Forms.ToolStrip();
            this.lblPLAdmin = new System.Windows.Forms.Label();
            this.plRecetasTit = new System.Windows.Forms.Panel();
            this.lblIntroRec = new System.Windows.Forms.Label();
            this.lblTitReceta = new System.Windows.Forms.Label();
            this.pbChef = new System.Windows.Forms.PictureBox();
            this.plSeleccionProd = new System.Windows.Forms.Panel();
            this.cmbSeleccionProd = new System.Windows.Forms.ComboBox();
            this.lblProductoTit = new System.Windows.Forms.Label();
            this.lblSelProdTit = new System.Windows.Forms.Label();
            this.plIngredienteReceta = new System.Windows.Forms.Panel();
            this.plAnuncioIngre = new System.Windows.Forms.Panel();
            this.lblAnuncio = new System.Windows.Forms.Label();
            this.lblCantUni = new System.Windows.Forms.Label();
            this.nudCantUni = new System.Windows.Forms.NumericUpDown();
            this.btnAñadirReceta = new System.Windows.Forms.Button();
            this.cmbIngrediente = new System.Windows.Forms.ComboBox();
            this.lblIngrediente = new System.Windows.Forms.Label();
            this.lblTitIngreReceta = new System.Windows.Forms.Label();
            this.dtgInfoReceta = new System.Windows.Forms.DataGridView();
            this.Column1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column2 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column3 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column4 = new System.Windows.Forms.DataGridViewButtonColumn();
            this.lblProductReceta = new System.Windows.Forms.Label();
            this.lblRecetaPara = new System.Windows.Forms.Label();
            this.btnGuardarReceta = new System.Windows.Forms.Button();
            this.panel1.SuspendLayout();
            this.plRecetasTit.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbChef)).BeginInit();
            this.plSeleccionProd.SuspendLayout();
            this.plIngredienteReceta.SuspendLayout();
            this.plAnuncioIngre.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantUni)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dtgInfoReceta)).BeginInit();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.Controls.Add(this.btnSalir);
            this.panel1.Controls.Add(this.tsP);
            this.panel1.Controls.Add(this.lblPLAdmin);
            this.panel1.Location = new System.Drawing.Point(0, 2);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(1203, 94);
            this.panel1.TabIndex = 8;
            // 
            // btnSalir
            // 
            this.btnSalir.Font = new System.Drawing.Font("Constantia", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnSalir.Location = new System.Drawing.Point(1142, 21);
            this.btnSalir.Name = "btnSalir";
            this.btnSalir.Size = new System.Drawing.Size(37, 37);
            this.btnSalir.TabIndex = 6;
            this.btnSalir.Text = "➡️";
            this.btnSalir.UseVisualStyleBackColor = true;
            // 
            // tsP
            // 
            this.tsP.Dock = System.Windows.Forms.DockStyle.None;
            this.tsP.Font = new System.Drawing.Font("Constantia", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tsP.Location = new System.Drawing.Point(90, 54);
            this.tsP.Name = "tsP";
            this.tsP.Size = new System.Drawing.Size(111, 25);
            this.tsP.TabIndex = 5;
            this.tsP.Text = "toolStrip1";
            // 
            // lblPLAdmin
            // 
            this.lblPLAdmin.AutoSize = true;
            this.lblPLAdmin.Font = new System.Drawing.Font("Constantia", 14.25F);
            this.lblPLAdmin.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(195)))), ((int)(((byte)(143)))), ((int)(((byte)(14)))));
            this.lblPLAdmin.Location = new System.Drawing.Point(86, 21);
            this.lblPLAdmin.Name = "lblPLAdmin";
            this.lblPLAdmin.Size = new System.Drawing.Size(309, 23);
            this.lblPLAdmin.TabIndex = 4;
            this.lblPLAdmin.Text = "Panel Administrador - Family Bakery";
            // 
            // plRecetasTit
            // 
            this.plRecetasTit.BackColor = System.Drawing.Color.White;
            this.plRecetasTit.Controls.Add(this.lblIntroRec);
            this.plRecetasTit.Controls.Add(this.lblTitReceta);
            this.plRecetasTit.Controls.Add(this.pbChef);
            this.plRecetasTit.Location = new System.Drawing.Point(0, 92);
            this.plRecetasTit.Name = "plRecetasTit";
            this.plRecetasTit.Size = new System.Drawing.Size(1200, 75);
            this.plRecetasTit.TabIndex = 9;
            // 
            // lblIntroRec
            // 
            this.lblIntroRec.AutoSize = true;
            this.lblIntroRec.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblIntroRec.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(121)))));
            this.lblIntroRec.Location = new System.Drawing.Point(87, 41);
            this.lblIntroRec.Name = "lblIntroRec";
            this.lblIntroRec.Size = new System.Drawing.Size(313, 15);
            this.lblIntroRec.TabIndex = 2;
            this.lblIntroRec.Text = "Definir composición de ingredientes para cada producto";
            this.lblIntroRec.Click += new System.EventHandler(this.lblIntroRec_Click);
            // 
            // lblTitReceta
            // 
            this.lblTitReceta.AutoSize = true;
            this.lblTitReceta.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblTitReceta.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(184)))), ((int)(((byte)(124)))), ((int)(((byte)(14)))));
            this.lblTitReceta.Location = new System.Drawing.Point(86, 17);
            this.lblTitReceta.Name = "lblTitReceta";
            this.lblTitReceta.Size = new System.Drawing.Size(151, 20);
            this.lblTitReceta.TabIndex = 1;
            this.lblTitReceta.Text = "Gestión de Recetas";
            // 
            // pbChef
            // 
            this.pbChef.Image = ((System.Drawing.Image)(resources.GetObject("pbChef.Image")));
            this.pbChef.Location = new System.Drawing.Point(46, 17);
            this.pbChef.Name = "pbChef";
            this.pbChef.Size = new System.Drawing.Size(34, 30);
            this.pbChef.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pbChef.TabIndex = 0;
            this.pbChef.TabStop = false;
            // 
            // plSeleccionProd
            // 
            this.plSeleccionProd.BackColor = System.Drawing.Color.White;
            this.plSeleccionProd.Controls.Add(this.cmbSeleccionProd);
            this.plSeleccionProd.Controls.Add(this.lblProductoTit);
            this.plSeleccionProd.Controls.Add(this.lblSelProdTit);
            this.plSeleccionProd.Location = new System.Drawing.Point(64, 190);
            this.plSeleccionProd.Name = "plSeleccionProd";
            this.plSeleccionProd.Size = new System.Drawing.Size(1076, 138);
            this.plSeleccionProd.TabIndex = 10;
            // 
            // cmbSeleccionProd
            // 
            this.cmbSeleccionProd.FormattingEnabled = true;
            this.cmbSeleccionProd.Location = new System.Drawing.Point(32, 80);
            this.cmbSeleccionProd.Name = "cmbSeleccionProd";
            this.cmbSeleccionProd.Size = new System.Drawing.Size(1017, 21);
            this.cmbSeleccionProd.TabIndex = 2;
            this.cmbSeleccionProd.Text = "--Seleccionar Producto--";
            // 
            // lblProductoTit
            // 
            this.lblProductoTit.AutoSize = true;
            this.lblProductoTit.Font = new System.Drawing.Font("Microsoft Sans Serif", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblProductoTit.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(54)))), ((int)(((byte)(65)))), ((int)(((byte)(83)))));
            this.lblProductoTit.Location = new System.Drawing.Point(30, 59);
            this.lblProductoTit.Name = "lblProductoTit";
            this.lblProductoTit.Size = new System.Drawing.Size(69, 18);
            this.lblProductoTit.TabIndex = 1;
            this.lblProductoTit.Text = "Producto";
            // 
            // lblSelProdTit
            // 
            this.lblSelProdTit.AutoSize = true;
            this.lblSelProdTit.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblSelProdTit.Location = new System.Drawing.Point(29, 16);
            this.lblSelProdTit.Name = "lblSelProdTit";
            this.lblSelProdTit.Size = new System.Drawing.Size(160, 20);
            this.lblSelProdTit.TabIndex = 0;
            this.lblSelProdTit.Text = "Seleccionar Producto";
            // 
            // plIngredienteReceta
            // 
            this.plIngredienteReceta.BackColor = System.Drawing.Color.White;
            this.plIngredienteReceta.Controls.Add(this.plAnuncioIngre);
            this.plIngredienteReceta.Controls.Add(this.lblCantUni);
            this.plIngredienteReceta.Controls.Add(this.nudCantUni);
            this.plIngredienteReceta.Controls.Add(this.btnAñadirReceta);
            this.plIngredienteReceta.Controls.Add(this.cmbIngrediente);
            this.plIngredienteReceta.Controls.Add(this.lblIngrediente);
            this.plIngredienteReceta.Controls.Add(this.lblTitIngreReceta);
            this.plIngredienteReceta.Controls.Add(this.dtgInfoReceta);
            this.plIngredienteReceta.Controls.Add(this.lblProductReceta);
            this.plIngredienteReceta.Controls.Add(this.lblRecetaPara);
            this.plIngredienteReceta.Location = new System.Drawing.Point(64, 341);
            this.plIngredienteReceta.Name = "plIngredienteReceta";
            this.plIngredienteReceta.Size = new System.Drawing.Size(1076, 346);
            this.plIngredienteReceta.TabIndex = 11;
            // 
            // plAnuncioIngre
            // 
            this.plAnuncioIngre.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plAnuncioIngre.Controls.Add(this.lblAnuncio);
            this.plAnuncioIngre.Location = new System.Drawing.Point(27, 52);
            this.plAnuncioIngre.Name = "plAnuncioIngre";
            this.plAnuncioIngre.Size = new System.Drawing.Size(1028, 34);
            this.plAnuncioIngre.TabIndex = 10;
            // 
            // lblAnuncio
            // 
            this.lblAnuncio.AutoSize = true;
            this.lblAnuncio.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblAnuncio.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(106)))), ((int)(((byte)(114)))), ((int)(((byte)(130)))));
            this.lblAnuncio.Location = new System.Drawing.Point(363, 9);
            this.lblAnuncio.Name = "lblAnuncio";
            this.lblAnuncio.Size = new System.Drawing.Size(349, 15);
            this.lblAnuncio.TabIndex = 0;
            this.lblAnuncio.Text = "No hay ingredientes en esta receta. Agrega ingredientes abajo.";
            // 
            // lblCantUni
            // 
            this.lblCantUni.AutoSize = true;
            this.lblCantUni.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(54)))), ((int)(((byte)(65)))), ((int)(((byte)(83)))));
            this.lblCantUni.Location = new System.Drawing.Point(344, 274);
            this.lblCantUni.Name = "lblCantUni";
            this.lblCantUni.Size = new System.Drawing.Size(160, 13);
            this.lblCantUni.TabIndex = 9;
            this.lblCantUni.Text = "Cantidad Requerida (por unidad)";
            // 
            // nudCantUni
            // 
            this.nudCantUni.Location = new System.Drawing.Point(347, 301);
            this.nudCantUni.Name = "nudCantUni";
            this.nudCantUni.Size = new System.Drawing.Size(333, 20);
            this.nudCantUni.TabIndex = 8;
            // 
            // btnAñadirReceta
            // 
            this.btnAñadirReceta.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnAñadirReceta.ForeColor = System.Drawing.Color.White;
            this.btnAñadirReceta.Location = new System.Drawing.Point(710, 299);
            this.btnAñadirReceta.Name = "btnAñadirReceta";
            this.btnAñadirReceta.Size = new System.Drawing.Size(333, 23);
            this.btnAñadirReceta.TabIndex = 7;
            this.btnAñadirReceta.Text = "+ Añadir Receta";
            this.btnAñadirReceta.UseVisualStyleBackColor = false;
            // 
            // cmbIngrediente
            // 
            this.cmbIngrediente.FormattingEnabled = true;
            this.cmbIngrediente.Location = new System.Drawing.Point(26, 301);
            this.cmbIngrediente.Name = "cmbIngrediente";
            this.cmbIngrediente.Size = new System.Drawing.Size(295, 21);
            this.cmbIngrediente.TabIndex = 5;
            this.cmbIngrediente.Text = "--Seleccionar--";
            // 
            // lblIngrediente
            // 
            this.lblIngrediente.AutoSize = true;
            this.lblIngrediente.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(54)))), ((int)(((byte)(65)))), ((int)(((byte)(83)))));
            this.lblIngrediente.Location = new System.Drawing.Point(24, 274);
            this.lblIngrediente.Name = "lblIngrediente";
            this.lblIngrediente.Size = new System.Drawing.Size(60, 13);
            this.lblIngrediente.TabIndex = 4;
            this.lblIngrediente.Text = "Ingrediente";
            // 
            // lblTitIngreReceta
            // 
            this.lblTitIngreReceta.AutoSize = true;
            this.lblTitIngreReceta.Location = new System.Drawing.Point(24, 242);
            this.lblTitIngreReceta.Name = "lblTitIngreReceta";
            this.lblTitIngreReceta.Size = new System.Drawing.Size(151, 13);
            this.lblTitIngreReceta.TabIndex = 3;
            this.lblTitIngreReceta.Text = "Añadir Ingrediente a la Receta";
            // 
            // dtgInfoReceta
            // 
            this.dtgInfoReceta.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dtgInfoReceta.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Column1,
            this.Column2,
            this.Column3,
            this.Column4});
            this.dtgInfoReceta.Location = new System.Drawing.Point(26, 92);
            this.dtgInfoReceta.Name = "dtgInfoReceta";
            this.dtgInfoReceta.Size = new System.Drawing.Size(1029, 81);
            this.dtgInfoReceta.TabIndex = 2;
            // 
            // Column1
            // 
            this.Column1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column1.HeaderText = "Ingrediente";
            this.Column1.Name = "Column1";
            // 
            // Column2
            // 
            this.Column2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column2.HeaderText = "Cantidad Requerida";
            this.Column2.Name = "Column2";
            // 
            // Column3
            // 
            this.Column3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column3.HeaderText = "Unidad";
            this.Column3.Name = "Column3";
            // 
            // Column4
            // 
            this.Column4.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column4.HeaderText = "Acción";
            this.Column4.Name = "Column4";
            // 
            // lblProductReceta
            // 
            this.lblProductReceta.AutoSize = true;
            this.lblProductReceta.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.lblProductReceta.Location = new System.Drawing.Point(101, 27);
            this.lblProductReceta.Name = "lblProductReceta";
            this.lblProductReceta.Size = new System.Drawing.Size(50, 13);
            this.lblProductReceta.TabIndex = 1;
            this.lblProductReceta.Text = "Producto";
            // 
            // lblRecetaPara
            // 
            this.lblRecetaPara.AutoSize = true;
            this.lblRecetaPara.Location = new System.Drawing.Point(24, 27);
            this.lblRecetaPara.Name = "lblRecetaPara";
            this.lblRecetaPara.Size = new System.Drawing.Size(69, 13);
            this.lblRecetaPara.TabIndex = 0;
            this.lblRecetaPara.Text = "Receta para:";
            // 
            // btnGuardarReceta
            // 
            this.btnGuardarReceta.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnGuardarReceta.ForeColor = System.Drawing.Color.White;
            this.btnGuardarReceta.Location = new System.Drawing.Point(789, 703);
            this.btnGuardarReceta.Name = "btnGuardarReceta";
            this.btnGuardarReceta.Size = new System.Drawing.Size(351, 34);
            this.btnGuardarReceta.TabIndex = 12;
            this.btnGuardarReceta.Text = "💾 Guardar Receta Completa";
            this.btnGuardarReceta.UseVisualStyleBackColor = false;
            // 
            // FormReceta
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1200, 749);
            this.Controls.Add(this.btnGuardarReceta);
            this.Controls.Add(this.plIngredienteReceta);
            this.Controls.Add(this.plSeleccionProd);
            this.Controls.Add(this.plRecetasTit);
            this.Controls.Add(this.panel1);
            this.Name = "FormReceta";
            this.Text = "FormReceta";
            this.Load += new System.EventHandler(this.FormReceta_Load);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.plRecetasTit.ResumeLayout(false);
            this.plRecetasTit.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbChef)).EndInit();
            this.plSeleccionProd.ResumeLayout(false);
            this.plSeleccionProd.PerformLayout();
            this.plIngredienteReceta.ResumeLayout(false);
            this.plIngredienteReceta.PerformLayout();
            this.plAnuncioIngre.ResumeLayout(false);
            this.plAnuncioIngre.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantUni)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dtgInfoReceta)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button btnSalir;
        private System.Windows.Forms.ToolStrip tsP;
        private System.Windows.Forms.Label lblPLAdmin;
        private System.Windows.Forms.Panel plRecetasTit;
        private System.Windows.Forms.Label lblIntroRec;
        private System.Windows.Forms.Label lblTitReceta;
        private System.Windows.Forms.PictureBox pbChef;
        private System.Windows.Forms.Panel plSeleccionProd;
        private System.Windows.Forms.ComboBox cmbSeleccionProd;
        private System.Windows.Forms.Label lblProductoTit;
        private System.Windows.Forms.Label lblSelProdTit;
        private System.Windows.Forms.Panel plIngredienteReceta;
        private System.Windows.Forms.DataGridView dtgInfoReceta;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column1;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column2;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column3;
        private System.Windows.Forms.DataGridViewButtonColumn Column4;
        private System.Windows.Forms.Label lblProductReceta;
        private System.Windows.Forms.Label lblRecetaPara;
        private System.Windows.Forms.Label lblCantUni;
        private System.Windows.Forms.NumericUpDown nudCantUni;
        private System.Windows.Forms.Button btnAñadirReceta;
        private System.Windows.Forms.ComboBox cmbIngrediente;
        private System.Windows.Forms.Label lblIngrediente;
        private System.Windows.Forms.Label lblTitIngreReceta;
        private System.Windows.Forms.Button btnGuardarReceta;
        private System.Windows.Forms.Panel plAnuncioIngre;
        private System.Windows.Forms.Label lblAnuncio;
    }
}