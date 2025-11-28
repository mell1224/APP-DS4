namespace Pedidos
{
    partial class FormProduccion
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormProduccion));
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnSalir = new System.Windows.Forms.Button();
            this.tsP = new System.Windows.Forms.ToolStrip();
            this.lblPLAdmin = new System.Windows.Forms.Label();
            this.plProduccionTit = new System.Windows.Forms.Panel();
            this.lblIntroProduccion = new System.Windows.Forms.Label();
            this.lblTitProduccion = new System.Windows.Forms.Label();
            this.pbProducc = new System.Windows.Forms.PictureBox();
            this.plLote = new System.Windows.Forms.Panel();
            this.plInfoLote = new System.Windows.Forms.Panel();
            this.plIngreStock = new System.Windows.Forms.Panel();
            this.lblIngreNec = new System.Windows.Forms.Label();
            this.dtgIngreNecDisp = new System.Windows.Forms.DataGridView();
            this.Column1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column2 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column3 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.lblTitSDespProd = new System.Windows.Forms.Label();
            this.btnRegProd = new System.Windows.Forms.Button();
            this.txtObserv = new System.Windows.Forms.TextBox();
            this.lblObserv = new System.Windows.Forms.Label();
            this.nudCantProd = new System.Windows.Forms.NumericUpDown();
            this.lblCantAProducir = new System.Windows.Forms.Label();
            this.plStockActual = new System.Windows.Forms.Panel();
            this.lblPrecUni = new System.Windows.Forms.Label();
            this.lblCostUni = new System.Windows.Forms.Label();
            this.pbStock = new System.Windows.Forms.PictureBox();
            this.lblUnidStock = new System.Windows.Forms.Label();
            this.lblTitStock = new System.Windows.Forms.Label();
            this.cmbSelecProd = new System.Windows.Forms.ComboBox();
            this.lblProdProducir = new System.Windows.Forms.Label();
            this.lblLoteProd = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.lblUniDProd = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.plProduccionTit.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbProducc)).BeginInit();
            this.plLote.SuspendLayout();
            this.plInfoLote.SuspendLayout();
            this.plIngreStock.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dtgIngreNecDisp)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantProd)).BeginInit();
            this.plStockActual.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbStock)).BeginInit();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.Controls.Add(this.btnSalir);
            this.panel1.Controls.Add(this.tsP);
            this.panel1.Controls.Add(this.lblPLAdmin);
            this.panel1.Location = new System.Drawing.Point(1, 1);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(1203, 94);
            this.panel1.TabIndex = 9;
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
            // plProduccionTit
            // 
            this.plProduccionTit.BackColor = System.Drawing.Color.White;
            this.plProduccionTit.Controls.Add(this.lblIntroProduccion);
            this.plProduccionTit.Controls.Add(this.lblTitProduccion);
            this.plProduccionTit.Controls.Add(this.pbProducc);
            this.plProduccionTit.Location = new System.Drawing.Point(1, 92);
            this.plProduccionTit.Name = "plProduccionTit";
            this.plProduccionTit.Size = new System.Drawing.Size(1200, 78);
            this.plProduccionTit.TabIndex = 10;
            // 
            // lblIntroProduccion
            // 
            this.lblIntroProduccion.AutoSize = true;
            this.lblIntroProduccion.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblIntroProduccion.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(121)))));
            this.lblIntroProduccion.Location = new System.Drawing.Point(87, 41);
            this.lblIntroProduccion.Name = "lblIntroProduccion";
            this.lblIntroProduccion.Size = new System.Drawing.Size(287, 15);
            this.lblIntroProduccion.TabIndex = 2;
            this.lblIntroProduccion.Text = "Registrar lotes de producción y actualizar inventario";
            // 
            // lblTitProduccion
            // 
            this.lblTitProduccion.AutoSize = true;
            this.lblTitProduccion.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblTitProduccion.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(184)))), ((int)(((byte)(124)))), ((int)(((byte)(14)))));
            this.lblTitProduccion.Location = new System.Drawing.Point(86, 17);
            this.lblTitProduccion.Name = "lblTitProduccion";
            this.lblTitProduccion.Size = new System.Drawing.Size(174, 20);
            this.lblTitProduccion.TabIndex = 1;
            this.lblTitProduccion.Text = "Registro de Producción";
            // 
            // pbProducc
            // 
            this.pbProducc.Location = new System.Drawing.Point(46, 17);
            this.pbProducc.Name = "pbProducc";
            this.pbProducc.Size = new System.Drawing.Size(34, 30);
            this.pbProducc.TabIndex = 0;
            this.pbProducc.TabStop = false;
            // 
            // plLote
            // 
            this.plLote.BackColor = System.Drawing.Color.White;
            this.plLote.Controls.Add(this.label1);
            this.plLote.Controls.Add(this.plInfoLote);
            this.plLote.Controls.Add(this.cmbSelecProd);
            this.plLote.Controls.Add(this.lblProdProducir);
            this.plLote.Controls.Add(this.lblLoteProd);
            this.plLote.Location = new System.Drawing.Point(249, 190);
            this.plLote.Name = "plLote";
            this.plLote.Size = new System.Drawing.Size(711, 547);
            this.plLote.TabIndex = 11;
            // 
            // plInfoLote
            // 
            this.plInfoLote.Controls.Add(this.lblUniDProd);
            this.plInfoLote.Controls.Add(this.label2);
            this.plInfoLote.Controls.Add(this.plIngreStock);
            this.plInfoLote.Controls.Add(this.lblTitSDespProd);
            this.plInfoLote.Controls.Add(this.btnRegProd);
            this.plInfoLote.Controls.Add(this.txtObserv);
            this.plInfoLote.Controls.Add(this.lblObserv);
            this.plInfoLote.Controls.Add(this.nudCantProd);
            this.plInfoLote.Controls.Add(this.lblCantAProducir);
            this.plInfoLote.Controls.Add(this.plStockActual);
            this.plInfoLote.Location = new System.Drawing.Point(26, 147);
            this.plInfoLote.Name = "plInfoLote";
            this.plInfoLote.Size = new System.Drawing.Size(646, 385);
            this.plInfoLote.TabIndex = 3;
            // 
            // plIngreStock
            // 
            this.plIngreStock.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(240)))), ((int)(((byte)(253)))), ((int)(((byte)(244)))));
            this.plIngreStock.Controls.Add(this.lblIngreNec);
            this.plIngreStock.Controls.Add(this.dtgIngreNecDisp);
            this.plIngreStock.Location = new System.Drawing.Point(23, 173);
            this.plIngreStock.Name = "plIngreStock";
            this.plIngreStock.Size = new System.Drawing.Size(601, 100);
            this.plIngreStock.TabIndex = 8;
            // 
            // lblIngreNec
            // 
            this.lblIngreNec.AutoSize = true;
            this.lblIngreNec.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(13)))), ((int)(((byte)(84)))), ((int)(((byte)(114)))));
            this.lblIngreNec.Location = new System.Drawing.Point(17, 9);
            this.lblIngreNec.Name = "lblIngreNec";
            this.lblIngreNec.Size = new System.Drawing.Size(121, 13);
            this.lblIngreNec.TabIndex = 1;
            this.lblIngreNec.Text = "Ingredientes Necesarios";
            // 
            // dtgIngreNecDisp
            // 
            this.dtgIngreNecDisp.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dtgIngreNecDisp.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Column1,
            this.Column2,
            this.Column3});
            this.dtgIngreNecDisp.Location = new System.Drawing.Point(20, 25);
            this.dtgIngreNecDisp.Name = "dtgIngreNecDisp";
            this.dtgIngreNecDisp.Size = new System.Drawing.Size(563, 62);
            this.dtgIngreNecDisp.TabIndex = 0;
            // 
            // Column1
            // 
            this.Column1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column1.HeaderText = "";
            this.Column1.Name = "Column1";
            // 
            // Column2
            // 
            this.Column2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column2.HeaderText = "Necesario";
            this.Column2.Name = "Column2";
            // 
            // Column3
            // 
            this.Column3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column3.HeaderText = "Disponible";
            this.Column3.Name = "Column3";
            // 
            // lblTitSDespProd
            // 
            this.lblTitSDespProd.AutoSize = true;
            this.lblTitSDespProd.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitSDespProd.Location = new System.Drawing.Point(20, 151);
            this.lblTitSDespProd.Name = "lblTitSDespProd";
            this.lblTitSDespProd.Size = new System.Drawing.Size(152, 13);
            this.lblTitSDespProd.TabIndex = 7;
            this.lblTitSDespProd.Text = "Stock después de producción:";
            // 
            // btnRegProd
            // 
            this.btnRegProd.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnRegProd.ForeColor = System.Drawing.Color.White;
            this.btnRegProd.Location = new System.Drawing.Point(382, 339);
            this.btnRegProd.Name = "btnRegProd";
            this.btnRegProd.Size = new System.Drawing.Size(242, 33);
            this.btnRegProd.TabIndex = 6;
            this.btnRegProd.Text = "📝 Registrar Producción ";
            this.btnRegProd.UseVisualStyleBackColor = false;
            // 
            // txtObserv
            // 
            this.txtObserv.Location = new System.Drawing.Point(23, 297);
            this.txtObserv.Multiline = true;
            this.txtObserv.Name = "txtObserv";
            this.txtObserv.Size = new System.Drawing.Size(601, 39);
            this.txtObserv.TabIndex = 5;
            // 
            // lblObserv
            // 
            this.lblObserv.AutoSize = true;
            this.lblObserv.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(64)))), ((int)(((byte)(64)))), ((int)(((byte)(64)))));
            this.lblObserv.Location = new System.Drawing.Point(20, 281);
            this.lblObserv.Name = "lblObserv";
            this.lblObserv.Size = new System.Drawing.Size(129, 13);
            this.lblObserv.TabIndex = 4;
            this.lblObserv.Text = "Observaciones (Opcional)";
            // 
            // nudCantProd
            // 
            this.nudCantProd.Location = new System.Drawing.Point(23, 128);
            this.nudCantProd.Name = "nudCantProd";
            this.nudCantProd.Size = new System.Drawing.Size(601, 20);
            this.nudCantProd.TabIndex = 3;
            // 
            // lblCantAProducir
            // 
            this.lblCantAProducir.AutoSize = true;
            this.lblCantAProducir.Location = new System.Drawing.Point(20, 112);
            this.lblCantAProducir.Name = "lblCantAProducir";
            this.lblCantAProducir.Size = new System.Drawing.Size(103, 13);
            this.lblCantAProducir.TabIndex = 2;
            this.lblCantAProducir.Text = "Cantidad a Producir ";
            // 
            // plStockActual
            // 
            this.plStockActual.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plStockActual.Controls.Add(this.lblPrecUni);
            this.plStockActual.Controls.Add(this.lblCostUni);
            this.plStockActual.Controls.Add(this.pbStock);
            this.plStockActual.Controls.Add(this.lblUnidStock);
            this.plStockActual.Controls.Add(this.lblTitStock);
            this.plStockActual.Location = new System.Drawing.Point(23, 18);
            this.plStockActual.Name = "plStockActual";
            this.plStockActual.Size = new System.Drawing.Size(601, 78);
            this.plStockActual.TabIndex = 1;
            // 
            // lblPrecUni
            // 
            this.lblPrecUni.AutoSize = true;
            this.lblPrecUni.Location = new System.Drawing.Point(548, 44);
            this.lblPrecUni.Name = "lblPrecUni";
            this.lblPrecUni.Size = new System.Drawing.Size(34, 13);
            this.lblPrecUni.TabIndex = 4;
            this.lblPrecUni.Text = "$0.00";
            // 
            // lblCostUni
            // 
            this.lblCostUni.AutoSize = true;
            this.lblCostUni.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblCostUni.Location = new System.Drawing.Point(464, 12);
            this.lblCostUni.Name = "lblCostUni";
            this.lblCostUni.Size = new System.Drawing.Size(119, 13);
            this.lblCostUni.TabIndex = 3;
            this.lblCostUni.Text = "Costo Unitario Estimado";
            // 
            // pbStock
            // 
            this.pbStock.BackColor = System.Drawing.Color.Gainsboro;
            this.pbStock.Image = ((System.Drawing.Image)(resources.GetObject("pbStock.Image")));
            this.pbStock.Location = new System.Drawing.Point(20, 41);
            this.pbStock.Name = "pbStock";
            this.pbStock.Size = new System.Drawing.Size(21, 16);
            this.pbStock.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pbStock.TabIndex = 2;
            this.pbStock.TabStop = false;
            // 
            // lblUnidStock
            // 
            this.lblUnidStock.AutoSize = true;
            this.lblUnidStock.Location = new System.Drawing.Point(47, 44);
            this.lblUnidStock.Name = "lblUnidStock";
            this.lblUnidStock.Size = new System.Drawing.Size(59, 13);
            this.lblUnidStock.TabIndex = 1;
            this.lblUnidStock.Text = "0 unidades";
            // 
            // lblTitStock
            // 
            this.lblTitStock.AutoSize = true;
            this.lblTitStock.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitStock.Location = new System.Drawing.Point(17, 12);
            this.lblTitStock.Name = "lblTitStock";
            this.lblTitStock.Size = new System.Drawing.Size(68, 13);
            this.lblTitStock.TabIndex = 0;
            this.lblTitStock.Text = "Stock Actual";
            // 
            // cmbSelecProd
            // 
            this.cmbSelecProd.FormattingEnabled = true;
            this.cmbSelecProd.Location = new System.Drawing.Point(34, 94);
            this.cmbSelecProd.Name = "cmbSelecProd";
            this.cmbSelecProd.Size = new System.Drawing.Size(639, 21);
            this.cmbSelecProd.TabIndex = 2;
            this.cmbSelecProd.Text = "-- Seleccionar Producto--";
            this.cmbSelecProd.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
            // 
            // lblProdProducir
            // 
            this.lblProdProducir.AutoSize = true;
            this.lblProdProducir.Location = new System.Drawing.Point(31, 61);
            this.lblProdProducir.Name = "lblProdProducir";
            this.lblProdProducir.Size = new System.Drawing.Size(104, 13);
            this.lblProdProducir.TabIndex = 1;
            this.lblProdProducir.Text = "Producto a Producir ";
            // 
            // lblLoteProd
            // 
            this.lblLoteProd.AutoSize = true;
            this.lblLoteProd.Location = new System.Drawing.Point(31, 23);
            this.lblLoteProd.Name = "lblLoteProd";
            this.lblLoteProd.Size = new System.Drawing.Size(135, 13);
            this.lblLoteProd.TabIndex = 0;
            this.lblLoteProd.Text = "Nuevo Lote de Producción";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.label1.Location = new System.Drawing.Point(131, 61);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(11, 13);
            this.label1.TabIndex = 5;
            this.label1.Text = "*";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.label2.Location = new System.Drawing.Point(118, 112);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(11, 13);
            this.label2.TabIndex = 6;
            this.label2.Text = "*";
            // 
            // lblUniDProd
            // 
            this.lblUniDProd.AutoSize = true;
            this.lblUniDProd.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(228)))), ((int)(((byte)(125)))), ((int)(((byte)(4)))));
            this.lblUniDProd.Location = new System.Drawing.Point(178, 151);
            this.lblUniDProd.Name = "lblUniDProd";
            this.lblUniDProd.Size = new System.Drawing.Size(59, 13);
            this.lblUniDProd.TabIndex = 9;
            this.lblUniDProd.Text = "0 unidades";
            // 
            // FormProduccion
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1200, 749);
            this.Controls.Add(this.plLote);
            this.Controls.Add(this.plProduccionTit);
            this.Controls.Add(this.panel1);
            this.Name = "FormProduccion";
            this.Text = "FormProduccion";
            this.Load += new System.EventHandler(this.FormProduccion_Load);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.plProduccionTit.ResumeLayout(false);
            this.plProduccionTit.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbProducc)).EndInit();
            this.plLote.ResumeLayout(false);
            this.plLote.PerformLayout();
            this.plInfoLote.ResumeLayout(false);
            this.plInfoLote.PerformLayout();
            this.plIngreStock.ResumeLayout(false);
            this.plIngreStock.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dtgIngreNecDisp)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantProd)).EndInit();
            this.plStockActual.ResumeLayout(false);
            this.plStockActual.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbStock)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button btnSalir;
        private System.Windows.Forms.ToolStrip tsP;
        private System.Windows.Forms.Label lblPLAdmin;
        private System.Windows.Forms.Panel plProduccionTit;
        private System.Windows.Forms.Label lblIntroProduccion;
        private System.Windows.Forms.Label lblTitProduccion;
        private System.Windows.Forms.PictureBox pbProducc;
        private System.Windows.Forms.Panel plLote;
        private System.Windows.Forms.ComboBox cmbSelecProd;
        private System.Windows.Forms.Label lblProdProducir;
        private System.Windows.Forms.Label lblLoteProd;
        private System.Windows.Forms.Panel plInfoLote;
        private System.Windows.Forms.Label lblTitStock;
        private System.Windows.Forms.Panel plStockActual;
        private System.Windows.Forms.TextBox txtObserv;
        private System.Windows.Forms.Label lblObserv;
        private System.Windows.Forms.NumericUpDown nudCantProd;
        private System.Windows.Forms.Label lblCantAProducir;
        private System.Windows.Forms.Label lblPrecUni;
        private System.Windows.Forms.Label lblCostUni;
        private System.Windows.Forms.PictureBox pbStock;
        private System.Windows.Forms.Label lblUnidStock;
        private System.Windows.Forms.Button btnRegProd;
        private System.Windows.Forms.Label lblTitSDespProd;
        private System.Windows.Forms.Panel plIngreStock;
        private System.Windows.Forms.DataGridView dtgIngreNecDisp;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column1;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column2;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column3;
        private System.Windows.Forms.Label lblIngreNec;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label lblUniDProd;
    }
}