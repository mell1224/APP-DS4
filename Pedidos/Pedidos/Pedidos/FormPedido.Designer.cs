namespace Pedidos
{
    partial class FormPedido
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
            this.dataGridView1 = new System.Windows.Forms.DataGridView();
            this.idPedido = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.fecha = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.nomCli = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.total = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.dirEntrega = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.obser = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.metodoPago = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.estado = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.detalle = new System.Windows.Forms.DataGridViewButtonColumn();
            this.accion = new System.Windows.Forms.DataGridViewButtonColumn();
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnSalir = new System.Windows.Forms.Button();
            this.tsP = new System.Windows.Forms.ToolStrip();
            this.lblPLAdmin = new System.Windows.Forms.Label();
            this.lblPAdm = new System.Windows.Forms.Label();
            this.plEstadoP = new System.Windows.Forms.Panel();
            this.btnPreparado = new System.Windows.Forms.Button();
            this.btnPendiente = new System.Windows.Forms.Button();
            this.btnCompletado = new System.Windows.Forms.Button();
            this.btnTodosAd = new System.Windows.Forms.Button();
            this.lblTitGestionP = new System.Windows.Forms.Label();
            this.plVerPedido = new System.Windows.Forms.Panel();
            this.lblPrecioTot = new System.Windows.Forms.Label();
            this.lblTotSelProd = new System.Windows.Forms.Label();
            this.plProductoDetalle = new System.Windows.Forms.Panel();
            this.pbProducto = new System.Windows.Forms.PictureBox();
            this.lblPrecioProd = new System.Windows.Forms.Label();
            this.lblCantidadProd = new System.Windows.Forms.Label();
            this.lblNProduct = new System.Windows.Forms.Label();
            this.lblProduct = new System.Windows.Forms.Label();
            this.plMetodoP = new System.Windows.Forms.Panel();
            this.lblPago = new System.Windows.Forms.Label();
            this.lblTitPago = new System.Windows.Forms.Label();
            this.plEstado = new System.Windows.Forms.Panel();
            this.lblResulEstado = new System.Windows.Forms.Label();
            this.lblTitEstado = new System.Windows.Forms.Label();
            this.plTotal = new System.Windows.Forms.Panel();
            this.lblTotal = new System.Windows.Forms.Label();
            this.lblTitTotal = new System.Windows.Forms.Label();
            this.plClient = new System.Windows.Forms.Panel();
            this.lblNameClient = new System.Windows.Forms.Label();
            this.lblTitClien = new System.Windows.Forms.Label();
            this.lblDetalleP = new System.Windows.Forms.Label();
            this.lblNumeP = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
            this.panel1.SuspendLayout();
            this.plEstadoP.SuspendLayout();
            this.plVerPedido.SuspendLayout();
            this.plProductoDetalle.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbProducto)).BeginInit();
            this.plMetodoP.SuspendLayout();
            this.plEstado.SuspendLayout();
            this.plTotal.SuspendLayout();
            this.plClient.SuspendLayout();
            this.SuspendLayout();
            // 
            // dataGridView1
            // 
            this.dataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.idPedido,
            this.fecha,
            this.nomCli,
            this.total,
            this.dirEntrega,
            this.obser,
            this.metodoPago,
            this.estado,
            this.detalle,
            this.accion});
            this.dataGridView1.Location = new System.Drawing.Point(-4, 157);
            this.dataGridView1.Name = "dataGridView1";
            this.dataGridView1.Size = new System.Drawing.Size(1203, 90);
            this.dataGridView1.TabIndex = 8;
            // 
            // idPedido
            // 
            this.idPedido.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.idPedido.HeaderText = "ID Pedido";
            this.idPedido.Name = "idPedido";
            // 
            // fecha
            // 
            this.fecha.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.fecha.HeaderText = "Fecha Pedido";
            this.fecha.Name = "fecha";
            // 
            // nomCli
            // 
            this.nomCli.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.nomCli.HeaderText = "Nombre Cliente";
            this.nomCli.Name = "nomCli";
            // 
            // total
            // 
            this.total.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.total.HeaderText = "Total";
            this.total.Name = "total";
            // 
            // dirEntrega
            // 
            this.dirEntrega.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.dirEntrega.HeaderText = "Dirección Entrega";
            this.dirEntrega.Name = "dirEntrega";
            // 
            // obser
            // 
            this.obser.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.obser.HeaderText = "Observaciones";
            this.obser.Name = "obser";
            // 
            // metodoPago
            // 
            this.metodoPago.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.metodoPago.HeaderText = "Método Pago";
            this.metodoPago.Name = "metodoPago";
            // 
            // estado
            // 
            this.estado.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.estado.HeaderText = "Estado";
            this.estado.Name = "estado";
            // 
            // detalle
            // 
            this.detalle.HeaderText = "Detalle del Pedido";
            this.detalle.Name = "detalle";
            this.detalle.Resizable = System.Windows.Forms.DataGridViewTriState.True;
            this.detalle.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic;
            // 
            // accion
            // 
            this.accion.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.accion.HeaderText = "Acción";
            this.accion.Name = "accion";
            this.accion.UseColumnTextForButtonValue = true;
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.Controls.Add(this.btnSalir);
            this.panel1.Controls.Add(this.tsP);
            this.panel1.Controls.Add(this.lblPLAdmin);
            this.panel1.Location = new System.Drawing.Point(-4, -2);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(1203, 94);
            this.panel1.TabIndex = 7;
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
            // lblPAdm
            // 
            this.lblPAdm.AutoSize = true;
            this.lblPAdm.Font = new System.Drawing.Font("Constantia", 14.25F);
            this.lblPAdm.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(217)))), ((int)(((byte)(103)))), ((int)(((byte)(4)))));
            this.lblPAdm.Location = new System.Drawing.Point(86, 21);
            this.lblPAdm.Name = "lblPAdm";
            this.lblPAdm.Size = new System.Drawing.Size(309, 23);
            this.lblPAdm.TabIndex = 4;
            this.lblPAdm.Text = "Panel Administrador - Family Bakery";
            // 
            // plEstadoP
            // 
            this.plEstadoP.BackColor = System.Drawing.Color.White;
            this.plEstadoP.Controls.Add(this.btnPreparado);
            this.plEstadoP.Controls.Add(this.btnPendiente);
            this.plEstadoP.Controls.Add(this.btnCompletado);
            this.plEstadoP.Controls.Add(this.btnTodosAd);
            this.plEstadoP.Location = new System.Drawing.Point(697, 98);
            this.plEstadoP.Name = "plEstadoP";
            this.plEstadoP.Size = new System.Drawing.Size(486, 40);
            this.plEstadoP.TabIndex = 9;
            // 
            // btnPreparado
            // 
            this.btnPreparado.Font = new System.Drawing.Font("Constantia", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnPreparado.Location = new System.Drawing.Point(245, 6);
            this.btnPreparado.Name = "btnPreparado";
            this.btnPreparado.Size = new System.Drawing.Size(115, 31);
            this.btnPreparado.TabIndex = 3;
            this.btnPreparado.Text = "Preparado";
            this.btnPreparado.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnPreparado.UseVisualStyleBackColor = true;
            // 
            // btnPendiente
            // 
            this.btnPendiente.Font = new System.Drawing.Font("Constantia", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnPendiente.Location = new System.Drawing.Point(124, 6);
            this.btnPendiente.Name = "btnPendiente";
            this.btnPendiente.Size = new System.Drawing.Size(115, 31);
            this.btnPendiente.TabIndex = 2;
            this.btnPendiente.Text = "Pendiente";
            this.btnPendiente.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnPendiente.UseVisualStyleBackColor = true;
            // 
            // btnCompletado
            // 
            this.btnCompletado.Font = new System.Drawing.Font("Constantia", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnCompletado.Location = new System.Drawing.Point(366, 6);
            this.btnCompletado.Name = "btnCompletado";
            this.btnCompletado.Size = new System.Drawing.Size(115, 31);
            this.btnCompletado.TabIndex = 1;
            this.btnCompletado.Text = "Completado";
            this.btnCompletado.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCompletado.UseVisualStyleBackColor = true;
            // 
            // btnTodosAd
            // 
            this.btnTodosAd.Font = new System.Drawing.Font("Constantia", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnTodosAd.Location = new System.Drawing.Point(3, 6);
            this.btnTodosAd.Name = "btnTodosAd";
            this.btnTodosAd.Size = new System.Drawing.Size(115, 31);
            this.btnTodosAd.TabIndex = 0;
            this.btnTodosAd.Text = "Todos";
            this.btnTodosAd.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnTodosAd.UseVisualStyleBackColor = true;
            // 
            // lblTitGestionP
            // 
            this.lblTitGestionP.AutoSize = true;
            this.lblTitGestionP.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblTitGestionP.Location = new System.Drawing.Point(3, 98);
            this.lblTitGestionP.Name = "lblTitGestionP";
            this.lblTitGestionP.Size = new System.Drawing.Size(175, 24);
            this.lblTitGestionP.TabIndex = 10;
            this.lblTitGestionP.Text = "Gestión de Pedidos";
            // 
            // plVerPedido
            // 
            this.plVerPedido.BackColor = System.Drawing.Color.White;
            this.plVerPedido.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.plVerPedido.Controls.Add(this.lblNumeP);
            this.plVerPedido.Controls.Add(this.lblPrecioTot);
            this.plVerPedido.Controls.Add(this.lblTotSelProd);
            this.plVerPedido.Controls.Add(this.plProductoDetalle);
            this.plVerPedido.Controls.Add(this.lblProduct);
            this.plVerPedido.Controls.Add(this.plMetodoP);
            this.plVerPedido.Controls.Add(this.plEstado);
            this.plVerPedido.Controls.Add(this.plTotal);
            this.plVerPedido.Controls.Add(this.plClient);
            this.plVerPedido.Controls.Add(this.lblDetalleP);
            this.plVerPedido.Location = new System.Drawing.Point(339, 100);
            this.plVerPedido.Name = "plVerPedido";
            this.plVerPedido.Size = new System.Drawing.Size(548, 499);
            this.plVerPedido.TabIndex = 11;
            // 
            // lblPrecioTot
            // 
            this.lblPrecioTot.AutoSize = true;
            this.lblPrecioTot.Location = new System.Drawing.Point(455, 464);
            this.lblPrecioTot.Name = "lblPrecioTot";
            this.lblPrecioTot.Size = new System.Drawing.Size(43, 13);
            this.lblPrecioTot.TabIndex = 8;
            this.lblPrecioTot.Text = "$ 00.00";
            // 
            // lblTotSelProd
            // 
            this.lblTotSelProd.AutoSize = true;
            this.lblTotSelProd.Location = new System.Drawing.Point(24, 464);
            this.lblTotSelProd.Name = "lblTotSelProd";
            this.lblTotSelProd.Size = new System.Drawing.Size(31, 13);
            this.lblTotSelProd.TabIndex = 7;
            this.lblTotSelProd.Text = "Total";
            // 
            // plProductoDetalle
            // 
            this.plProductoDetalle.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plProductoDetalle.Controls.Add(this.pbProducto);
            this.plProductoDetalle.Controls.Add(this.lblPrecioProd);
            this.plProductoDetalle.Controls.Add(this.lblCantidadProd);
            this.plProductoDetalle.Controls.Add(this.lblNProduct);
            this.plProductoDetalle.Location = new System.Drawing.Point(21, 300);
            this.plProductoDetalle.Name = "plProductoDetalle";
            this.plProductoDetalle.Size = new System.Drawing.Size(492, 70);
            this.plProductoDetalle.TabIndex = 5;
            // 
            // pbProducto
            // 
            this.pbProducto.Location = new System.Drawing.Point(12, 15);
            this.pbProducto.Name = "pbProducto";
            this.pbProducto.Size = new System.Drawing.Size(55, 41);
            this.pbProducto.TabIndex = 3;
            this.pbProducto.TabStop = false;
            // 
            // lblPrecioProd
            // 
            this.lblPrecioProd.AutoSize = true;
            this.lblPrecioProd.Location = new System.Drawing.Point(431, 43);
            this.lblPrecioProd.Name = "lblPrecioProd";
            this.lblPrecioProd.Size = new System.Drawing.Size(37, 13);
            this.lblPrecioProd.TabIndex = 2;
            this.lblPrecioProd.Text = "$ 0.00";
            // 
            // lblCantidadProd
            // 
            this.lblCantidadProd.AutoSize = true;
            this.lblCantidadProd.Location = new System.Drawing.Point(92, 43);
            this.lblCantidadProd.Name = "lblCantidadProd";
            this.lblCantidadProd.Size = new System.Drawing.Size(61, 13);
            this.lblCantidadProd.TabIndex = 1;
            this.lblCantidadProd.Text = "Cantidad: 0";
            // 
            // lblNProduct
            // 
            this.lblNProduct.AutoSize = true;
            this.lblNProduct.Location = new System.Drawing.Point(92, 15);
            this.lblNProduct.Name = "lblNProduct";
            this.lblNProduct.Size = new System.Drawing.Size(90, 13);
            this.lblNProduct.TabIndex = 0;
            this.lblNProduct.Text = "Nombre Producto";
            // 
            // lblProduct
            // 
            this.lblProduct.AutoSize = true;
            this.lblProduct.Location = new System.Drawing.Point(23, 273);
            this.lblProduct.Name = "lblProduct";
            this.lblProduct.Size = new System.Drawing.Size(55, 13);
            this.lblProduct.TabIndex = 4;
            this.lblProduct.Text = "Productos";
            // 
            // plMetodoP
            // 
            this.plMetodoP.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plMetodoP.Controls.Add(this.lblPago);
            this.plMetodoP.Controls.Add(this.lblTitPago);
            this.plMetodoP.Location = new System.Drawing.Point(280, 151);
            this.plMetodoP.Name = "plMetodoP";
            this.plMetodoP.Size = new System.Drawing.Size(223, 81);
            this.plMetodoP.TabIndex = 3;
            // 
            // lblPago
            // 
            this.lblPago.AutoSize = true;
            this.lblPago.Location = new System.Drawing.Point(9, 50);
            this.lblPago.Name = "lblPago";
            this.lblPago.Size = new System.Drawing.Size(31, 13);
            this.lblPago.TabIndex = 1;
            this.lblPago.Text = "--------";
            // 
            // lblTitPago
            // 
            this.lblTitPago.AutoSize = true;
            this.lblTitPago.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitPago.Location = new System.Drawing.Point(9, 21);
            this.lblTitPago.Name = "lblTitPago";
            this.lblTitPago.Size = new System.Drawing.Size(86, 13);
            this.lblTitPago.TabIndex = 0;
            this.lblTitPago.Text = "Método de Pago";
            // 
            // plEstado
            // 
            this.plEstado.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plEstado.Controls.Add(this.lblResulEstado);
            this.plEstado.Controls.Add(this.lblTitEstado);
            this.plEstado.Location = new System.Drawing.Point(21, 151);
            this.plEstado.Name = "plEstado";
            this.plEstado.Size = new System.Drawing.Size(223, 81);
            this.plEstado.TabIndex = 2;
            // 
            // lblResulEstado
            // 
            this.lblResulEstado.AutoSize = true;
            this.lblResulEstado.Location = new System.Drawing.Point(9, 50);
            this.lblResulEstado.Name = "lblResulEstado";
            this.lblResulEstado.Size = new System.Drawing.Size(34, 13);
            this.lblResulEstado.TabIndex = 1;
            this.lblResulEstado.Text = "---------";
            // 
            // lblTitEstado
            // 
            this.lblTitEstado.AutoSize = true;
            this.lblTitEstado.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitEstado.Location = new System.Drawing.Point(9, 21);
            this.lblTitEstado.Name = "lblTitEstado";
            this.lblTitEstado.Size = new System.Drawing.Size(40, 13);
            this.lblTitEstado.TabIndex = 0;
            this.lblTitEstado.Text = "Estado";
            // 
            // plTotal
            // 
            this.plTotal.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plTotal.Controls.Add(this.lblTotal);
            this.plTotal.Controls.Add(this.lblTitTotal);
            this.plTotal.Location = new System.Drawing.Point(280, 52);
            this.plTotal.Name = "plTotal";
            this.plTotal.Size = new System.Drawing.Size(223, 81);
            this.plTotal.TabIndex = 2;
            // 
            // lblTotal
            // 
            this.lblTotal.AutoSize = true;
            this.lblTotal.Location = new System.Drawing.Point(9, 50);
            this.lblTotal.Name = "lblTotal";
            this.lblTotal.Size = new System.Drawing.Size(43, 13);
            this.lblTotal.TabIndex = 1;
            this.lblTotal.Text = "$ 00.00";
            // 
            // lblTitTotal
            // 
            this.lblTitTotal.AutoSize = true;
            this.lblTitTotal.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitTotal.Location = new System.Drawing.Point(9, 21);
            this.lblTitTotal.Name = "lblTitTotal";
            this.lblTitTotal.Size = new System.Drawing.Size(31, 13);
            this.lblTitTotal.TabIndex = 0;
            this.lblTitTotal.Text = "Total";
            // 
            // plClient
            // 
            this.plClient.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(249)))), ((int)(((byte)(250)))), ((int)(((byte)(251)))));
            this.plClient.Controls.Add(this.lblNameClient);
            this.plClient.Controls.Add(this.lblTitClien);
            this.plClient.Location = new System.Drawing.Point(21, 52);
            this.plClient.Name = "plClient";
            this.plClient.Size = new System.Drawing.Size(223, 81);
            this.plClient.TabIndex = 1;
            // 
            // lblNameClient
            // 
            this.lblNameClient.AutoSize = true;
            this.lblNameClient.Location = new System.Drawing.Point(9, 50);
            this.lblNameClient.Name = "lblNameClient";
            this.lblNameClient.Size = new System.Drawing.Size(44, 13);
            this.lblNameClient.TabIndex = 1;
            this.lblNameClient.Text = "Nombre";
            // 
            // lblTitClien
            // 
            this.lblTitClien.AutoSize = true;
            this.lblTitClien.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(74)))), ((int)(((byte)(85)))), ((int)(((byte)(101)))));
            this.lblTitClien.Location = new System.Drawing.Point(9, 21);
            this.lblTitClien.Name = "lblTitClien";
            this.lblTitClien.Size = new System.Drawing.Size(39, 13);
            this.lblTitClien.TabIndex = 0;
            this.lblTitClien.Text = "Cliente";
            // 
            // lblDetalleP
            // 
            this.lblDetalleP.AutoSize = true;
            this.lblDetalleP.Location = new System.Drawing.Point(24, 22);
            this.lblDetalleP.Name = "lblDetalleP";
            this.lblDetalleP.Size = new System.Drawing.Size(93, 13);
            this.lblDetalleP.TabIndex = 0;
            this.lblDetalleP.Text = "Detalle del Pedido";
            // 
            // lblNumeP
            // 
            this.lblNumeP.AutoSize = true;
            this.lblNumeP.Location = new System.Drawing.Point(123, 23);
            this.lblNumeP.Name = "lblNumeP";
            this.lblNumeP.Size = new System.Drawing.Size(38, 13);
            this.lblNumeP.TabIndex = 2;
            this.lblNumeP.Text = "#0000";
            // 
            // FormPedido
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1195, 690);
            this.Controls.Add(this.plVerPedido);
            this.Controls.Add(this.lblTitGestionP);
            this.Controls.Add(this.plEstadoP);
            this.Controls.Add(this.dataGridView1);
            this.Controls.Add(this.panel1);
            this.Name = "FormPedido";
            this.Text = "FormPedido";
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.plEstadoP.ResumeLayout(false);
            this.plVerPedido.ResumeLayout(false);
            this.plVerPedido.PerformLayout();
            this.plProductoDetalle.ResumeLayout(false);
            this.plProductoDetalle.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pbProducto)).EndInit();
            this.plMetodoP.ResumeLayout(false);
            this.plMetodoP.PerformLayout();
            this.plEstado.ResumeLayout(false);
            this.plEstado.PerformLayout();
            this.plTotal.ResumeLayout(false);
            this.plTotal.PerformLayout();
            this.plClient.ResumeLayout(false);
            this.plClient.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.DataGridView dataGridView1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label lblPLAdmin;
        private System.Windows.Forms.DataGridViewTextBoxColumn idPedido;
        private System.Windows.Forms.DataGridViewTextBoxColumn fecha;
        private System.Windows.Forms.DataGridViewTextBoxColumn nomCli;
        private System.Windows.Forms.DataGridViewTextBoxColumn total;
        private System.Windows.Forms.DataGridViewTextBoxColumn dirEntrega;
        private System.Windows.Forms.DataGridViewTextBoxColumn obser;
        private System.Windows.Forms.DataGridViewTextBoxColumn metodoPago;
        private System.Windows.Forms.DataGridViewTextBoxColumn estado;
        private System.Windows.Forms.DataGridViewButtonColumn detalle;
        private System.Windows.Forms.DataGridViewButtonColumn accion;
        private System.Windows.Forms.Label lblPAdm;
        private System.Windows.Forms.ToolStrip tsP;
        private System.Windows.Forms.Button btnSalir;
        private System.Windows.Forms.Panel plEstadoP;
        private System.Windows.Forms.Button btnPreparado;
        private System.Windows.Forms.Button btnPendiente;
        private System.Windows.Forms.Button btnCompletado;
        private System.Windows.Forms.Button btnTodosAd;
        private System.Windows.Forms.Label lblTitGestionP;
        private System.Windows.Forms.Panel plVerPedido;
        private System.Windows.Forms.Label lblProduct;
        private System.Windows.Forms.Panel plMetodoP;
        private System.Windows.Forms.Label lblPago;
        private System.Windows.Forms.Label lblTitPago;
        private System.Windows.Forms.Panel plEstado;
        private System.Windows.Forms.Label lblResulEstado;
        private System.Windows.Forms.Label lblTitEstado;
        private System.Windows.Forms.Panel plTotal;
        private System.Windows.Forms.Label lblTotal;
        private System.Windows.Forms.Label lblTitTotal;
        private System.Windows.Forms.Panel plClient;
        private System.Windows.Forms.Label lblNameClient;
        private System.Windows.Forms.Label lblTitClien;
        private System.Windows.Forms.Label lblDetalleP;
        private System.Windows.Forms.Label lblPrecioTot;
        private System.Windows.Forms.Label lblTotSelProd;
        private System.Windows.Forms.Panel plProductoDetalle;
        private System.Windows.Forms.PictureBox pbProducto;
        private System.Windows.Forms.Label lblPrecioProd;
        private System.Windows.Forms.Label lblCantidadProd;
        private System.Windows.Forms.Label lblNProduct;
        private System.Windows.Forms.Label lblNumeP;
    }
}
