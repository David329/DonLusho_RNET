namespace TFIA
{
    partial class Form1
    {
        /// <summary>
        /// Variable del diseñador necesaria.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Limpiar los recursos que se estén usando.
        /// </summary>
        /// <param name="disposing">true si los recursos administrados se deben desechar; false en caso contrario.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Código generado por el Diseñador de Windows Forms

        /// <summary>
        /// Método necesario para admitir el Diseñador. No se puede modificar
        /// el contenido de este método con el editor de código.
        /// </summary>
        private void InitializeComponent()
        {
            this.txtretweets = new System.Windows.Forms.TextBox();
            this.txtverificada = new System.Windows.Forms.TextBox();
            this.txtseguidores = new System.Windows.Forms.TextBox();
            this.txtamigos = new System.Windows.Forms.TextBox();
            this.txtfavoritos = new System.Windows.Forms.TextBox();
            this.txtmenciones = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.btnprocesar = new System.Windows.Forms.Button();
            this.btngenerar = new System.Windows.Forms.Button();
            this.label7 = new System.Windows.Forms.Label();
            this.txtresultado = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // txtretweets
            // 
            this.txtretweets.Location = new System.Drawing.Point(313, 56);
            this.txtretweets.Name = "txtretweets";
            this.txtretweets.Size = new System.Drawing.Size(230, 22);
            this.txtretweets.TabIndex = 0;
            // 
            // txtverificada
            // 
            this.txtverificada.Location = new System.Drawing.Point(313, 130);
            this.txtverificada.Name = "txtverificada";
            this.txtverificada.Size = new System.Drawing.Size(230, 22);
            this.txtverificada.TabIndex = 0;
            // 
            // txtseguidores
            // 
            this.txtseguidores.Location = new System.Drawing.Point(313, 205);
            this.txtseguidores.Name = "txtseguidores";
            this.txtseguidores.Size = new System.Drawing.Size(230, 22);
            this.txtseguidores.TabIndex = 0;
            // 
            // txtamigos
            // 
            this.txtamigos.Location = new System.Drawing.Point(313, 294);
            this.txtamigos.Name = "txtamigos";
            this.txtamigos.Size = new System.Drawing.Size(230, 22);
            this.txtamigos.TabIndex = 0;
            // 
            // txtfavoritos
            // 
            this.txtfavoritos.Location = new System.Drawing.Point(313, 391);
            this.txtfavoritos.Name = "txtfavoritos";
            this.txtfavoritos.Size = new System.Drawing.Size(230, 22);
            this.txtfavoritos.TabIndex = 0;
            // 
            // txtmenciones
            // 
            this.txtmenciones.Location = new System.Drawing.Point(313, 488);
            this.txtmenciones.Name = "txtmenciones";
            this.txtmenciones.Size = new System.Drawing.Size(230, 22);
            this.txtmenciones.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(37, 56);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(66, 17);
            this.label1.TabIndex = 1;
            this.label1.Text = "Retweets";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(37, 130);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(71, 17);
            this.label2.TabIndex = 1;
            this.label2.Text = "Verificada";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(37, 205);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(80, 17);
            this.label3.TabIndex = 1;
            this.label3.Text = "Seguidores";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(37, 294);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(54, 17);
            this.label4.TabIndex = 1;
            this.label4.Text = "Amigos";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(37, 391);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(66, 17);
            this.label5.TabIndex = 1;
            this.label5.Text = "Favoritos";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(37, 488);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(76, 17);
            this.label6.TabIndex = 1;
            this.label6.Text = "Menciones";
            // 
            // btnprocesar
            // 
            this.btnprocesar.Location = new System.Drawing.Point(40, 596);
            this.btnprocesar.Name = "btnprocesar";
            this.btnprocesar.Size = new System.Drawing.Size(150, 23);
            this.btnprocesar.TabIndex = 2;
            this.btnprocesar.Text = "Procesar";
            this.btnprocesar.UseVisualStyleBackColor = true;
            this.btnprocesar.Click += new System.EventHandler(this.btnprocesar_Click);
            // 
            // btngenerar
            // 
            this.btngenerar.Location = new System.Drawing.Point(313, 596);
            this.btngenerar.Name = "btngenerar";
            this.btngenerar.Size = new System.Drawing.Size(150, 23);
            this.btngenerar.TabIndex = 2;
            this.btngenerar.Text = "Generar";
            this.btngenerar.UseVisualStyleBackColor = true;
            this.btngenerar.Click += new System.EventHandler(this.btngenerar_Click);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(40, 666);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(64, 17);
            this.label7.TabIndex = 3;
            this.label7.Text = "Resultdo";
            // 
            // txtresultado
            // 
            this.txtresultado.Location = new System.Drawing.Point(160, 666);
            this.txtresultado.Name = "txtresultado";
            this.txtresultado.ReadOnly = true;
            this.txtresultado.Size = new System.Drawing.Size(383, 22);
            this.txtresultado.TabIndex = 4;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(677, 744);
            this.Controls.Add(this.txtresultado);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.btngenerar);
            this.Controls.Add(this.btnprocesar);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.txtmenciones);
            this.Controls.Add(this.txtfavoritos);
            this.Controls.Add(this.txtamigos);
            this.Controls.Add(this.txtseguidores);
            this.Controls.Add(this.txtverificada);
            this.Controls.Add(this.txtretweets);
            this.Name = "Form1";
            this.Text = "TF - IA";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox txtretweets;
        private System.Windows.Forms.TextBox txtverificada;
        private System.Windows.Forms.TextBox txtseguidores;
        private System.Windows.Forms.TextBox txtamigos;
        private System.Windows.Forms.TextBox txtfavoritos;
        private System.Windows.Forms.TextBox txtmenciones;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Button btnprocesar;
        private System.Windows.Forms.Button btngenerar;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox txtresultado;
    }
}

