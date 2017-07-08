using RDotNet;
using System;
using System.Windows.Forms;

namespace TFIA
{
    public partial class Form1 : Form
    {
        Random r = new Random();
        string retweets, verificada, seguidores, amigos, favoritos, menciones;
        public Form1()
        {
            InitializeComponent();
        }

        private void btngenerar_Click(object sender, EventArgs e)
        {
            txtretweets.Text = r.Next(10, 700).ToString();
            txtverificada.Text = r.Next(0, 2) == 1 ? "Si" : "No";
            txtseguidores.Text = r.Next(1, 799).ToString();
            txtamigos.Text = r.Next(1, 899).ToString();
            txtfavoritos.Text = r.Next(1, 999).ToString();
            txtmenciones.Text = r.Next(1, 500).ToString();
        }
        private void btnprocesar_Click(object sender, EventArgs e)
        {
            if (!validar()) return;
            retweets = int.Parse(txtretweets.Text) > 450 ? "Muchos" : "Pocos";
            verificada = txtverificada.Text;
            seguidores = int.Parse(txtseguidores.Text) > 500 ? "Muchos" : "Pocos";
            amigos = int.Parse(txtamigos.Text) > 300 ? "Muchos" : "Pocos";
            favoritos = int.Parse(txtfavoritos.Text) > 400 ? "Muchos" : "Pocos";
            menciones = int.Parse(txtmenciones.Text) > 100 ? "Muchos" : "Pocos";

            //funcionLusho
            txtresultado.Text = DonLusho();
        }
        public bool validar()
        {
            if (txtmenciones.Text.Length == 0 || txtfavoritos.Text.Length == 0 || txtamigos.Text.Length == 0 || txtretweets.Text.Length == 0
                || txtverificada.Text.Length == 0 || txtseguidores.Text.Length == 0)
            {
                MessageBox.Show("Completar todos los campos");
                return false;
            }
            return true;
        }
        public string DonLusho()
        {
            REngine.SetEnvironmentVariables();
            REngine engine = REngine.GetInstance();
            // REngine requires explicit initialization.
            // You can set some parameters.
            engine.Initialize();
            //engine.Evaluate("setwd('C:/Users/DZZ/Documents')");
            engine.Evaluate(@"#IA TF
setwd('C:/Users/DZZ/Documents')
library(prob)

# Cantidad de Retweets realizados por un usuario
Retweets = data.frame(
  Retweets = c('Muchos','Pocos'),
  probs = c(0.30, 0.70)
)

# Indica si la cuenta es verificada o no
Verificada = data.frame(
  Verificada = c('Si','No'),
  probs = c(0.10, 0.90)
)

# Cantidad de Seguidores que tiene un usuario
Seguidores = data.frame(
  Seguidores = c('Muchos','Pocos'),
  probs = c(0.25, 0.75)
)

# Cantidad de Amigos que tiene un usuario
Amigos = data.frame(
  Amigos = c('Muchos','Pocos'),
  probs = c(0.45, 0.55)
)

# Cantidad de Amigos que tiene un usuario
Favoritos = data.frame(
  Favoritos = c('Muchos','Pocos'),
  probs = c(0.4, 0.6)
)

# Cantidad de Menciones realizadas al usuario
Menciones = data.frame(
  Menciones = c('Muchos','Pocos'),
  probs = c(0.35, 0.65)
)

# containsHashtag
# containslink

# Método para obtener data de un archivo
putExternalData <- function(X, fileName) {
  
  Datos <- read.table(file = fileName)
  
  X <- data.frame(X, Datos)
  X <- probspace(X)
  
  for(i in 1:128) {
    X$probs[i] = X$V1[i]/sum(X$V1)
  }
  return(X)
}

# Varibale a analizar
Malicioso = expand.grid(
  Retweets = c('Muchos','Pocos'),
  Verificada = c('Si','No'),
  Seguidores = c('Muchos','Pocos'),
  Amigos = c('Muchos','Pocos'),
  Favoritos = c('Muchos','Pocos'),
  Menciones = c('Muchos','Pocos'),
  Malicioso = c('Si','No')
)

# Carpeta por fefault Mis Documentos usar setwd() para cambiarla si se desea
Malicioso <- putExternalData(Malicioso, 'Data.txt')
");

            //VARIABLES 
            //CharacterVector charVec;
            //charVec= engine.CreateCharacterVector(new[] { retweets });
            //engine.SetSymbol("retw", charVec);
            //charVec = engine.CreateCharacterVector(new[] { verificada });
            //engine.SetSymbol("veri", charVec);
            //charVec = engine.CreateCharacterVector(new[] { seguidores });
            //engine.SetSymbol("segui", charVec);
            //charVec = engine.CreateCharacterVector(new[] { amigos });
            //engine.SetSymbol("amig", charVec);
            //charVec = engine.CreateCharacterVector(new[] { favoritos });
            //engine.SetSymbol("fav", charVec);
            //charVec = engine.CreateCharacterVector(new[] { menciones });
            //engine.SetSymbol("menc", charVec);

            string[] result = engine.Evaluate((Condicionales(retweets, verificada, seguidores, amigos, favoritos, menciones))).AsCharacter().ToArray();
            engine.Dispose();
            return "La probabilidad de tweet malicioso es de: "+ result[0];
        }

        #region Condicionales
        private string Condicionales(string retweets, string verificada, string seguidores, string amigos, string favoritos, string menciones)
        {
            string result = "";
            if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";


            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";
            //28

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";


            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            //52
            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";


            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";
            //57
            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'Si', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            //68
            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";
            //73
            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            //90
            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";


            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Muchos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Muchos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Muchos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            //105
            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Muchos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Muchos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Muchos') *
  Prob(Menciones, Menciones == 'Pocos')";
            //113
            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Muchos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Muchos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Muchos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            //121
            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Muchos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Muchos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Muchos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "Si" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'Si' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'Si') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Muchos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Muchos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Muchos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

            else if (retweets == "Pocos" && verificada == "No" && seguidores == "Pocos" && amigos == "Pocos" && favoritos == "Pocos" && menciones == "Pocos")
                result = @"Prob(
  Malicioso, Malicioso == 'No', 
  given = (Retweets == 'Pocos' & Verificada == 'No' & Seguidores == 'Pocos' & Amigos == 'Pocos' & Favoritos == 'Pocos' & Menciones == 'Pocos')) *
  Prob(Retweets, Retweets == 'Pocos') *
  Prob(Verificada, Verificada == 'No') *
  Prob(Seguidores, Seguidores == 'Pocos') *
  Prob(Amigos, Amigos == 'Pocos') *
  Prob(Favoritos, Favoritos == 'Pocos') *
  Prob(Menciones, Menciones == 'Pocos')";

                return result;
        }
        #endregion
    }
}