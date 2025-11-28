namespace Pedidos
{
    partial class FormInventario
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormInventario));
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnSalir = new System.Windows.Forms.Button();
            this.tsP = new System.Windows.Forms.ToolStrip();
            this.lblPLAdmin = new System.Windows.Forms.Label();
            this.plGesInventario = new System.Windows.Forms.Panel();
            this.btnNuevoIng = new System.Windows.Forms.Button();
            this.plProximoV = new System.Windows.Forms.Panel();
            this.pb2Warning = new System.Windows.Forms.PictureBox();
            this.label4 = new System.Windows.Forms.Label();
            this.lblProxiV = new System.Windows.Forms.Label();
            this.plStockB = new System.Windows.Forms.Panel();
            this.pbWarning1 = new System.Windows.Forms.PictureBox();
            this.lblIngrediente = new System.Windows.Forms.Label();
            this.lblStockb = new System.Windows.Forms.Label();
            this.lblGestion = new System.Windows.Forms.Label();
            this.dtgInventario = new System.Windows.Forms.DataGridView();
            this.Nombre = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Cantidad = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column2 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column3 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column4 = new System.Windows.Forms.DataGridViewButtonColumn();
            this.Column5 = new System.Windows.Forms.DataGridViewButtonColumn();
            this.plNuevoIngre = new System.Windows.Forms.Panel();
            this.btnGuardarIngre = new System.Windows.Forms.Button();
            this.btnCancelarIngre = new System.Windows.Forms.Button();
            this.mcalFechaIngre = new System.Windows.Forms.MonthCalendar();
            this.lblFechaIngre = new System.Windows.Forms.Label();
            this.nudStockIngre = new System.Windows.Forms.NumericUpDown();
            this.lblStockIngre = new System.Windows.Forms.Label();
            this.nudCantiIngre = new System.Windows.Forms.NumericUpDown();
            this.txtUniIngre = new System.Windows.Forms.TextBox();
            this.lblUniIngre = new System.Windows.Forms.Label();
            this.lblCantIngre = new System.Windows.Forms.Label();
            this.txtNombreIngre = new System.Windows.Forms.TextBox();
            this.lblNomIngre = new System.Windows.Forms.Label();
            this.lblNuevoI = new System.Windows.Forms.Label();
            this.plEditarIngrediente = new System.Windows.Forms.Panel();
            this.btnEditGuarI = new System.Windows.Forms.Button();
            this.btnEditCancI = new System.Windows.Forms.Button();
            this.mcalEditI = new System.Windows.Forms.MonthCalendar();
            this.lblEditFechaI = new System.Windows.Forms.Label();
            this.nudEditStockI = new System.Windows.Forms.NumericUpDown();
            this.lblEditStockI = new System.Windows.Forms.Label();
            this.nudEditCantI = new System.Windows.Forms.NumericUpDown();
            this.txtEditUniI = new System.Windows.Forms.TextBox();
            this.lblEditUniI = new System.Windows.Forms.Label();
            this.lblEditCanI = new System.Windows.Forms.Label();
            this.txtEditNomI = new System.Windows.Forms.TextBox();
            this.lblEditNomI = new System.Windows.Forms.Label();
            this.lblEditarIngre = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.plGesInventario.SuspendLayout();
            this.plProximoV.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pb2Warning)).BeginInit();
            this.plStockB.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbWarning1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dtgInventario)).BeginInit();
            this.plNuevoIngre.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudStockIngre)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantiIngre)).BeginInit();
            this.plEditarIngrediente.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudEditStockI)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEditCantI)).BeginInit();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.Controls.Add(this.btnSalir);
            this.panel1.Controls.Add(this.tsP);
            this.panel1.Controls.Add(this.lblPLAdmin);
            this.panel1.Location = new System.Drawing.Point(2, 12);
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
            // plGesInventario
            // 
            this.plGesInventario.Controls.Add(this.btnNuevoIng);
            this.plGesInventario.Controls.Add(this.plProximoV);
            this.plGesInventario.Controls.Add(this.plStockB);
            this.plGesInventario.Controls.Add(this.lblGestion);
            this.plGesInventario.Location = new System.Drawing.Point(2, 112);
            this.plGesInventario.Name = "plGesInventario";
            this.plGesInventario.Size = new System.Drawing.Size(1186, 159);
            this.plGesInventario.TabIndex = 9;
            // 
            // btnNuevoIng
            // 
            this.btnNuevoIng.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnNuevoIng.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnNuevoIng.ForeColor = System.Drawing.Color.White;
            this.btnNuevoIng.Location = new System.Drawing.Point(1010, 16);
            this.btnNuevoIng.Name = "btnNuevoIng";
            this.btnNuevoIng.Size = new System.Drawing.Size(169, 32);
            this.btnNuevoIng.TabIndex = 3;
            this.btnNuevoIng.Text = "+   Nuevo Ingrediente";
            this.btnNuevoIng.UseVisualStyleBackColor = false;
            // 
            // plProximoV
            // 
            this.plProximoV.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(254)))), ((int)(((byte)(252)))), ((int)(((byte)(232)))));
            this.plProximoV.Controls.Add(this.pb2Warning);
            this.plProximoV.Controls.Add(this.label4);
            this.plProximoV.Controls.Add(this.lblProxiV);
            this.plProximoV.Location = new System.Drawing.Point(630, 61);
            this.plProximoV.Name = "plProximoV";
            this.plProximoV.Size = new System.Drawing.Size(556, 83);
            this.plProximoV.TabIndex = 2;
            // 
            // pb2Warning
            // 
            this.pb2Warning.Image = ((System.Drawing.Image)(resources.GetObject("pb2Warning.Image")));
            this.pb2Warning.Location = new System.Drawing.Point(15, 20);
            this.pb2Warning.Name = "pb2Warning";
            this.pb2Warning.Size = new System.Drawing.Size(43, 43);
            this.pb2Warning.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pb2Warning.TabIndex = 3;
            this.pb2Warning.TabStop = false;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(166)))), ((int)(((byte)(95)))), ((int)(((byte)(0)))));
            this.label4.Location = new System.Drawing.Point(75, 50);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(74, 13);
            this.label4.TabIndex = 1;
            this.label4.Text = "0 Ingredientes";
            // 
            // lblProxiV
            // 
            this.lblProxiV.AutoSize = true;
            this.lblProxiV.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(115)))), ((int)(((byte)(62)))), ((int)(((byte)(10)))));
            this.lblProxiV.Location = new System.Drawing.Point(75, 14);
            this.lblProxiV.Name = "lblProxiV";
            this.lblProxiV.Size = new System.Drawing.Size(90, 13);
            this.lblProxiV.TabIndex = 0;
            this.lblProxiV.Text = "Próximo a Vencer";
            // 
            // plStockB
            // 
            this.plStockB.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(254)))), ((int)(((byte)(242)))), ((int)(((byte)(242)))));
            this.plStockB.Controls.Add(this.pbWarning1);
            this.plStockB.Controls.Add(this.lblIngrediente);
            this.plStockB.Controls.Add(this.lblStockb);
            this.plStockB.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(146)))), ((int)(((byte)(24)))), ((int)(((byte)(61)))));
            this.plStockB.Location = new System.Drawing.Point(14, 61);
            this.plStockB.Name = "plStockB";
            this.plStockB.Size = new System.Drawing.Size(556, 83);
            this.plStockB.TabIndex = 1;
            // 
            // pbWarning1
            // 
            this.pbWarning1.Image = ((System.Drawing.Image)(resources.GetObject("pbWarning1.Image")));
            this.pbWarning1.Location = new System.Drawing.Point(18, 20);
            this.pbWarning1.Name = "pbWarning1";
            this.pbWarning1.Size = new System.Drawing.Size(43, 43);
            this.pbWarning1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pbWarning1.TabIndex = 2;
            this.pbWarning1.TabStop = false;
            // 
            // lblIngrediente
            // 
            this.lblIngrediente.AutoSize = true;
            this.lblIngrediente.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(254)))), ((int)(((byte)(242)))), ((int)(((byte)(242)))));
            this.lblIngrediente.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblIngrediente.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(197)))), ((int)(((byte)(0)))), ((int)(((byte)(7)))));
            this.lblIngrediente.Location = new System.Drawing.Point(67, 50);
            this.lblIngrediente.Name = "lblIngrediente";
            this.lblIngrediente.Size = new System.Drawing.Size(85, 15);
            this.lblIngrediente.TabIndex = 1;
            this.lblIngrediente.Text = "0 Ingredientes";
            // 
            // lblStockb
            // 
            this.lblStockb.AutoSize = true;
            this.lblStockb.Location = new System.Drawing.Point(67, 14);
            this.lblStockb.Name = "lblStockb";
            this.lblStockb.Size = new System.Drawing.Size(59, 13);
            this.lblStockb.TabIndex = 0;
            this.lblStockb.Text = "Stock Bajo";
            // 
            // lblGestion
            // 
            this.lblGestion.AutoSize = true;
            this.lblGestion.Location = new System.Drawing.Point(11, 0);
            this.lblGestion.Name = "lblGestion";
            this.lblGestion.Size = new System.Drawing.Size(108, 13);
            this.lblGestion.TabIndex = 0;
            this.lblGestion.Text = "Gestión de Inventario";
            // 
            // dtgInventario
            // 
            this.dtgInventario.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dtgInventario.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Nombre,
            this.Cantidad,
            this.Column1,
            this.Column2,
            this.Column3,
            this.Column4,
            this.Column5});
            this.dtgInventario.Location = new System.Drawing.Point(2, 277);
            this.dtgInventario.Name = "dtgInventario";
            this.dtgInventario.Size = new System.Drawing.Size(1186, 150);
            this.dtgInventario.TabIndex = 10;
            // 
            // Nombre
            // 
            this.Nombre.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Nombre.HeaderText = "Nombre";
            this.Nombre.Name = "Nombre";
            // 
            // Cantidad
            // 
            this.Cantidad.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Cantidad.HeaderText = "Cantidad";
            this.Cantidad.Name = "Cantidad";
            // 
            // Column1
            // 
            this.Column1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column1.HeaderText = "Unidad";
            this.Column1.Name = "Column1";
            // 
            // Column2
            // 
            this.Column2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column2.HeaderText = "Stock Mínimo";
            this.Column2.Name = "Column2";
            // 
            // Column3
            // 
            this.Column3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column3.HeaderText = "Fecha de Vencimiento";
            this.Column3.Name = "Column3";
            // 
            // Column4
            // 
            this.Column4.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column4.HeaderText = "";
            this.Column4.Name = "Column4";
            // 
            // Column5
            // 
            this.Column5.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Column5.HeaderText = "";
            this.Column5.Name = "Column5";
            // 
            // plNuevoIngre
            // 
            this.plNuevoIngre.BackColor = System.Drawing.Color.White;
            this.plNuevoIngre.Controls.Add(this.btnGuardarIngre);
            this.plNuevoIngre.Controls.Add(this.btnCancelarIngre);
            this.plNuevoIngre.Controls.Add(this.mcalFechaIngre);
            this.plNuevoIngre.Controls.Add(this.lblFechaIngre);
            this.plNuevoIngre.Controls.Add(this.nudStockIngre);
            this.plNuevoIngre.Controls.Add(this.lblStockIngre);
            this.plNuevoIngre.Controls.Add(this.nudCantiIngre);
            this.plNuevoIngre.Controls.Add(this.txtUniIngre);
            this.plNuevoIngre.Controls.Add(this.lblUniIngre);
            this.plNuevoIngre.Controls.Add(this.lblCantIngre);
            this.plNuevoIngre.Controls.Add(this.txtNombreIngre);
            this.plNuevoIngre.Controls.Add(this.lblNomIngre);
            this.plNuevoIngre.Controls.Add(this.lblNuevoI);
            this.plNuevoIngre.Location = new System.Drawing.Point(346, 112);
            this.plNuevoIngre.Name = "plNuevoIngre";
            this.plNuevoIngre.Size = new System.Drawing.Size(425, 526);
            this.plNuevoIngre.TabIndex = 4;
            // 
            // btnGuardarIngre
            // 
            this.btnGuardarIngre.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnGuardarIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnGuardarIngre.ForeColor = System.Drawing.Color.White;
            this.btnGuardarIngre.Location = new System.Drawing.Point(233, 480);
            this.btnGuardarIngre.Name = "btnGuardarIngre";
            this.btnGuardarIngre.Size = new System.Drawing.Size(162, 31);
            this.btnGuardarIngre.TabIndex = 12;
            this.btnGuardarIngre.Text = "Guardar";
            this.btnGuardarIngre.UseVisualStyleBackColor = false;
            // 
            // btnCancelarIngre
            // 
            this.btnCancelarIngre.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(229)))), ((int)(((byte)(231)))), ((int)(((byte)(235)))));
            this.btnCancelarIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnCancelarIngre.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(75)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.btnCancelarIngre.Location = new System.Drawing.Point(33, 480);
            this.btnCancelarIngre.Name = "btnCancelarIngre";
            this.btnCancelarIngre.Size = new System.Drawing.Size(162, 31);
            this.btnCancelarIngre.TabIndex = 11;
            this.btnCancelarIngre.Text = "Cancelar";
            this.btnCancelarIngre.UseVisualStyleBackColor = false;
            // 
            // mcalFechaIngre
            // 
            this.mcalFechaIngre.Location = new System.Drawing.Point(107, 306);
            this.mcalFechaIngre.Name = "mcalFechaIngre";
            this.mcalFechaIngre.TabIndex = 10;
            // 
            // lblFechaIngre
            // 
            this.lblFechaIngre.AutoSize = true;
            this.lblFechaIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblFechaIngre.Location = new System.Drawing.Point(30, 285);
            this.lblFechaIngre.Name = "lblFechaIngre";
            this.lblFechaIngre.Size = new System.Drawing.Size(141, 16);
            this.lblFechaIngre.TabIndex = 9;
            this.lblFechaIngre.Text = "Fecha de Vencimiento";
            // 
            // nudStockIngre
            // 
            this.nudStockIngre.Location = new System.Drawing.Point(33, 241);
            this.nudStockIngre.Name = "nudStockIngre";
            this.nudStockIngre.Size = new System.Drawing.Size(364, 20);
            this.nudStockIngre.TabIndex = 8;
            // 
            // lblStockIngre
            // 
            this.lblStockIngre.AutoSize = true;
            this.lblStockIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblStockIngre.Location = new System.Drawing.Point(31, 212);
            this.lblStockIngre.Name = "lblStockIngre";
            this.lblStockIngre.Size = new System.Drawing.Size(87, 16);
            this.lblStockIngre.TabIndex = 7;
            this.lblStockIngre.Text = "Stock Mínimo";
            // 
            // nudCantiIngre
            // 
            this.nudCantiIngre.Location = new System.Drawing.Point(34, 165);
            this.nudCantiIngre.Name = "nudCantiIngre";
            this.nudCantiIngre.Size = new System.Drawing.Size(144, 20);
            this.nudCantiIngre.TabIndex = 6;
            // 
            // txtUniIngre
            // 
            this.txtUniIngre.Location = new System.Drawing.Point(251, 165);
            this.txtUniIngre.Name = "txtUniIngre";
            this.txtUniIngre.Size = new System.Drawing.Size(144, 20);
            this.txtUniIngre.TabIndex = 5;
            // 
            // lblUniIngre
            // 
            this.lblUniIngre.AutoSize = true;
            this.lblUniIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblUniIngre.Location = new System.Drawing.Point(248, 143);
            this.lblUniIngre.Name = "lblUniIngre";
            this.lblUniIngre.Size = new System.Drawing.Size(51, 16);
            this.lblUniIngre.TabIndex = 4;
            this.lblUniIngre.Text = "Unidad";
            // 
            // lblCantIngre
            // 
            this.lblCantIngre.AutoSize = true;
            this.lblCantIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblCantIngre.Location = new System.Drawing.Point(31, 143);
            this.lblCantIngre.Name = "lblCantIngre";
            this.lblCantIngre.Size = new System.Drawing.Size(61, 16);
            this.lblCantIngre.TabIndex = 3;
            this.lblCantIngre.Text = "Cantidad";
            // 
            // txtNombreIngre
            // 
            this.txtNombreIngre.Location = new System.Drawing.Point(33, 91);
            this.txtNombreIngre.Multiline = true;
            this.txtNombreIngre.Name = "txtNombreIngre";
            this.txtNombreIngre.Size = new System.Drawing.Size(362, 33);
            this.txtNombreIngre.TabIndex = 2;
            // 
            // lblNomIngre
            // 
            this.lblNomIngre.AutoSize = true;
            this.lblNomIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblNomIngre.Location = new System.Drawing.Point(30, 61);
            this.lblNomIngre.Name = "lblNomIngre";
            this.lblNomIngre.Size = new System.Drawing.Size(56, 16);
            this.lblNomIngre.TabIndex = 1;
            this.lblNomIngre.Text = "Nombre";
            // 
            // lblNuevoI
            // 
            this.lblNuevoI.AutoSize = true;
            this.lblNuevoI.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblNuevoI.Location = new System.Drawing.Point(30, 16);
            this.lblNuevoI.Name = "lblNuevoI";
            this.lblNuevoI.Size = new System.Drawing.Size(166, 24);
            this.lblNuevoI.TabIndex = 0;
            this.lblNuevoI.Text = "Nuevo Ingrediente";
            // 
            // plEditarIngrediente
            // 
            this.plEditarIngrediente.BackColor = System.Drawing.Color.White;
            this.plEditarIngrediente.Controls.Add(this.btnEditGuarI);
            this.plEditarIngrediente.Controls.Add(this.btnEditCancI);
            this.plEditarIngrediente.Controls.Add(this.mcalEditI);
            this.plEditarIngrediente.Controls.Add(this.lblEditFechaI);
            this.plEditarIngrediente.Controls.Add(this.nudEditStockI);
            this.plEditarIngrediente.Controls.Add(this.lblEditStockI);
            this.plEditarIngrediente.Controls.Add(this.nudEditCantI);
            this.plEditarIngrediente.Controls.Add(this.txtEditUniI);
            this.plEditarIngrediente.Controls.Add(this.lblEditUniI);
            this.plEditarIngrediente.Controls.Add(this.lblEditCanI);
            this.plEditarIngrediente.Controls.Add(this.txtEditNomI);
            this.plEditarIngrediente.Controls.Add(this.lblEditNomI);
            this.plEditarIngrediente.Controls.Add(this.lblEditarIngre);
            this.plEditarIngrediente.Location = new System.Drawing.Point(777, 212);
            this.plEditarIngrediente.Name = "plEditarIngrediente";
            this.plEditarIngrediente.Size = new System.Drawing.Size(425, 526);
            this.plEditarIngrediente.TabIndex = 13;
            // 
            // btnEditGuarI
            // 
            this.btnEditGuarI.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.btnEditGuarI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnEditGuarI.ForeColor = System.Drawing.Color.White;
            this.btnEditGuarI.Location = new System.Drawing.Point(233, 480);
            this.btnEditGuarI.Name = "btnEditGuarI";
            this.btnEditGuarI.Size = new System.Drawing.Size(162, 31);
            this.btnEditGuarI.TabIndex = 12;
            this.btnEditGuarI.Text = "Guardar";
            this.btnEditGuarI.UseVisualStyleBackColor = false;
            // 
            // btnEditCancI
            // 
            this.btnEditCancI.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(229)))), ((int)(((byte)(231)))), ((int)(((byte)(235)))));
            this.btnEditCancI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnEditCancI.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(75)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.btnEditCancI.Location = new System.Drawing.Point(33, 480);
            this.btnEditCancI.Name = "btnEditCancI";
            this.btnEditCancI.Size = new System.Drawing.Size(162, 31);
            this.btnEditCancI.TabIndex = 11;
            this.btnEditCancI.Text = "Cancelar";
            this.btnEditCancI.UseVisualStyleBackColor = false;
            // 
            // mcalEditI
            // 
            this.mcalEditI.Location = new System.Drawing.Point(107, 306);
            this.mcalEditI.Name = "mcalEditI";
            this.mcalEditI.TabIndex = 10;
            // 
            // lblEditFechaI
            // 
            this.lblEditFechaI.AutoSize = true;
            this.lblEditFechaI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditFechaI.Location = new System.Drawing.Point(30, 285);
            this.lblEditFechaI.Name = "lblEditFechaI";
            this.lblEditFechaI.Size = new System.Drawing.Size(141, 16);
            this.lblEditFechaI.TabIndex = 9;
            this.lblEditFechaI.Text = "Fecha de Vencimiento";
            // 
            // nudEditStockI
            // 
            this.nudEditStockI.Location = new System.Drawing.Point(33, 241);
            this.nudEditStockI.Name = "nudEditStockI";
            this.nudEditStockI.Size = new System.Drawing.Size(364, 20);
            this.nudEditStockI.TabIndex = 8;
            // 
            // lblEditStockI
            // 
            this.lblEditStockI.AutoSize = true;
            this.lblEditStockI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditStockI.Location = new System.Drawing.Point(31, 212);
            this.lblEditStockI.Name = "lblEditStockI";
            this.lblEditStockI.Size = new System.Drawing.Size(87, 16);
            this.lblEditStockI.TabIndex = 7;
            this.lblEditStockI.Text = "Stock Mínimo";
            // 
            // nudEditCantI
            // 
            this.nudEditCantI.Location = new System.Drawing.Point(34, 165);
            this.nudEditCantI.Name = "nudEditCantI";
            this.nudEditCantI.Size = new System.Drawing.Size(144, 20);
            this.nudEditCantI.TabIndex = 6;
            // 
            // txtEditUniI
            // 
            this.txtEditUniI.Location = new System.Drawing.Point(251, 165);
            this.txtEditUniI.Name = "txtEditUniI";
            this.txtEditUniI.Size = new System.Drawing.Size(144, 20);
            this.txtEditUniI.TabIndex = 5;
            // 
            // lblEditUniI
            // 
            this.lblEditUniI.AutoSize = true;
            this.lblEditUniI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditUniI.Location = new System.Drawing.Point(248, 143);
            this.lblEditUniI.Name = "lblEditUniI";
            this.lblEditUniI.Size = new System.Drawing.Size(51, 16);
            this.lblEditUniI.TabIndex = 4;
            this.lblEditUniI.Text = "Unidad";
            // 
            // lblEditCanI
            // 
            this.lblEditCanI.AutoSize = true;
            this.lblEditCanI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditCanI.Location = new System.Drawing.Point(31, 143);
            this.lblEditCanI.Name = "lblEditCanI";
            this.lblEditCanI.Size = new System.Drawing.Size(61, 16);
            this.lblEditCanI.TabIndex = 3;
            this.lblEditCanI.Text = "Cantidad";
            // 
            // txtEditNomI
            // 
            this.txtEditNomI.Location = new System.Drawing.Point(33, 91);
            this.txtEditNomI.Multiline = true;
            this.txtEditNomI.Name = "txtEditNomI";
            this.txtEditNomI.Size = new System.Drawing.Size(362, 33);
            this.txtEditNomI.TabIndex = 2;
            // 
            // lblEditNomI
            // 
            this.lblEditNomI.AutoSize = true;
            this.lblEditNomI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditNomI.Location = new System.Drawing.Point(30, 61);
            this.lblEditNomI.Name = "lblEditNomI";
            this.lblEditNomI.Size = new System.Drawing.Size(56, 16);
            this.lblEditNomI.TabIndex = 1;
            this.lblEditNomI.Text = "Nombre";
            // 
            // lblEditarIngre
            // 
            this.lblEditarIngre.AutoSize = true;
            this.lblEditarIngre.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEditarIngre.Location = new System.Drawing.Point(30, 16);
            this.lblEditarIngre.Name = "lblEditarIngre";
            this.lblEditarIngre.Size = new System.Drawing.Size(158, 24);
            this.lblEditarIngre.TabIndex = 0;
            this.lblEditarIngre.Text = "Editar Ingrediente";
            // 
            // FormInventario
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1200, 749);
            this.Controls.Add(this.plEditarIngrediente);
            this.Controls.Add(this.plNuevoIngre);
            this.Controls.Add(this.dtgInventario);
            this.Controls.Add(this.plGesInventario);
            this.Controls.Add(this.panel1);
            this.Name = "FormInventario";
            this.Text = "FormInventario";
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.plGesInventario.ResumeLayout(false);
            this.plGesInventario.PerformLayout();
            this.plProximoV.ResumeLayout(false);
            this.plProximoV.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pb2Warning)).EndInit();
            this.plStockB.ResumeLayout(false);
            this.plStockB.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbWarning1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dtgInventario)).EndInit();
            this.plNuevoIngre.ResumeLayout(false);
            this.plNuevoIngre.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudStockIngre)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudCantiIngre)).EndInit();
            this.plEditarIngrediente.ResumeLayout(false);
            this.plEditarIngrediente.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudEditStockI)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudEditCantI)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button btnSalir;
        private System.Windows.Forms.ToolStrip tsP;
        private System.Windows.Forms.Label lblPLAdmin;
        private System.Windows.Forms.Panel plGesInventario;
        private System.Windows.Forms.Label lblGestion;
        private System.Windows.Forms.Panel plStockB;
        private System.Windows.Forms.Panel plProximoV;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label lblProxiV;
        private System.Windows.Forms.Label lblIngrediente;
        private System.Windows.Forms.Label lblStockb;
        private System.Windows.Forms.PictureBox pb2Warning;
        private System.Windows.Forms.DataGridView dtgInventario;
        private System.Windows.Forms.DataGridViewTextBoxColumn Nombre;
        private System.Windows.Forms.DataGridViewTextBoxColumn Cantidad;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column1;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column2;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column3;
        private System.Windows.Forms.DataGridViewButtonColumn Column4;
        private System.Windows.Forms.DataGridViewButtonColumn Column5;
        private System.Windows.Forms.PictureBox pbWarning1;
        private System.Windows.Forms.Panel plNuevoIngre;
        private System.Windows.Forms.Button btnNuevoIng;
        private System.Windows.Forms.Label lblNuevoI;
        private System.Windows.Forms.TextBox txtUniIngre;
        private System.Windows.Forms.Label lblUniIngre;
        private System.Windows.Forms.Label lblCantIngre;
        private System.Windows.Forms.TextBox txtNombreIngre;
        private System.Windows.Forms.Label lblNomIngre;
        private System.Windows.Forms.Button btnGuardarIngre;
        private System.Windows.Forms.Button btnCancelarIngre;
        private System.Windows.Forms.MonthCalendar mcalFechaIngre;
        private System.Windows.Forms.Label lblFechaIngre;
        private System.Windows.Forms.NumericUpDown nudStockIngre;
        private System.Windows.Forms.Label lblStockIngre;
        private System.Windows.Forms.NumericUpDown nudCantiIngre;
        private System.Windows.Forms.Panel plEditarIngrediente;
        private System.Windows.Forms.Button btnEditGuarI;
        private System.Windows.Forms.Button btnEditCancI;
        private System.Windows.Forms.MonthCalendar mcalEditI;
        private System.Windows.Forms.Label lblEditFechaI;
        private System.Windows.Forms.NumericUpDown nudEditStockI;
        private System.Windows.Forms.Label lblEditStockI;
        private System.Windows.Forms.NumericUpDown nudEditCantI;
        private System.Windows.Forms.TextBox txtEditUniI;
        private System.Windows.Forms.Label lblEditUniI;
        private System.Windows.Forms.Label lblEditCanI;
        private System.Windows.Forms.TextBox txtEditNomI;
        private System.Windows.Forms.Label lblEditNomI;
        private System.Windows.Forms.Label lblEditarIngre;
    }
}