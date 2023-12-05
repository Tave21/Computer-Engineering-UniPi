
<!DOCTYPE html>

<html lang="it">


<head>
      <meta charset="utf-8">
      <meta name="viewport" content="width = device-width">


    <title>Siti storici Lago di Garda</title>
	<link rel="stylesheet" type="text/css" href="../../css/style.css" />
	<script src="../../javascript/funz.js" ></script>
</head>
<?php
	session_start(); 
?>
<body >
	
	<!-- inizio top  -->
	<div >
		
		<!--  Inizio Menu navigazione -->
		<div >
			<ul>
        		<li><a href="../../index.php">Home</a></li>
        		<li><a href="reservation.php">Prenotazione Visite</a></li>
        		<li>
            		<a href="#">Social Newtork&#9662;</a>
            		<ul class="dropdown">
                		<li><a href="https://www.facebook.com/LagodiGardaItaly">Facebook</a></li>
                		<li><a href="https://twitter.com/LagoGardaPoint">Twitter</a></li>
            		</ul>
        		</li>
        		<li><a href="contacts.php">Contatti & Orari</a></li>
				<li><a href="">Media&#9662;</a>
            		<ul class="dropdown">
							<li><a href="foto.php">Foto Gallery</a></li>
							<li><a href="https://www.youtube.com/watch?v=IlYEwjc-o7c&list=PL944DF0F01F762B24">Video Gallery</a></li>
            		</ul>
				</li>
				
				<li>
				
					<?php
  				if(isset($_SESSION['logged'])) {
					  echo '<a href ="areaPersonale.php"> Area Personale</a>';
				  } else {
					  echo '<a href ="../login/index2.php"> Accedi o Registrati </a>';
				  }  
				?>

				</li>	
				<li>
            		<a href="#">Menu rapido&#9662;</a>
            		<ul class="dropdown">
                            <li><a href="recensioni.php">Recensioni</a></li>
                            <li><a href="storia.php">Storia</a></li>
							<li><a href="curiosita.php">Curiosità</a></li>
							<li><a href="newsletter.php">Newsletter</a></li>
							<li><a href="mappe.php">Maps</a></li>	
            		</ul>
        		</li>
						<?php
  				if(isset($_SESSION['logged'])) {
					  echo '
				<li>
			      <form name="logout_form" id="logout_form" method="post" action="../login/logout.php" enctype="multipart/form-data">
                        <label for="logout_button">
                          <input id="logout_button" class="sub_button" type="submit" name="logout" value="LOGOUT">
                        </label>   
                  </form>   
				</li>';
				  } 
				?>
    	    </ul>
  			
		</div>
			<!-- fine menu navigazione -->
		
		<!-- inizio titolo -->
		<div id="titolo">
			</div>
			<!-- fine titolo-->
			<h1 id="sps" style="text-align: center;"> CURIOSITÀ </h1>	
			
	</div>
	<!-- fine top -->

        <div style="background-color: antiquewhite;">
            <h2 class="caps">...il mostro Benacosaurus </h2>
            <p>
Lo sapevi che il Lago di Garda non è il nome originario del luogo?!
 Durante l’epoca romana il lago venne denominato “Benacus”, il nome del dio che personificava il lago stesso. La sua storia è parecchio travagliata, infatti durante le occupazioni barbariche cadde nelle mani dei longobardi e dei franchi di Carlo Magno. Con il tempo si costituirono i comuni e le signorie, insediate proprio lungo le sponde del lago, tutte in contrasto l’una all’altra.
 Da qui il nome Garda, di origine germanica che può essere tradotto con il termine “luogo di guardia”.
Tante sono le leggende e le storie che si raccontano che vedono come protagonista il lago, si narra di Benacosaurus, il mostro che abita le acque, avvistato nel 1965 e da allora si è mostrato in diverse occasioni in punti differenti del lago. I suoi fondali ad oggi inesplorati trasmettono ispirazione per fantasticare sulle strane creature che potrebbero viverci all’interno. 
            </p>
            <h2 class="caps">...le proprietà terapeutiche </h2>
            <p>
Qualcuno ti ha mai detto che il lago è conosciuto anche per le proprietà terapeutiche date dai componenti sodico-solfurei. 
Sin dal passato si erano notate bolle dal forte odore di zolfo affiorare la superficie, con il tempo si è cercato di canalizzare 
queste sorgenti subacquee perché potessero essere sfruttate come sorgenti termali accessibili da tutti. Solo durante il Novecento si riuscì nell’impresa, 
trasformando il lago oltre che ad una meta turistica di interesse per il suo paesaggio anche per le sue sorgenti termali.
            </p>
            <h2 class="caps">...il fiume più corto del mondo </h2>
            <p>
Lo sapevi che nel Lago di Garda sfocia il fiume più corto al mondo?!
 Si chiama Aril o Ri e si sviluppa per una lunghezza di 175m situato nel Comune di Malcesine a Cassone.
  Il luogo è stato immortalato nel 1913 in una bellissima opera d’arte “Chiesa a Cassone sul Garda” dipinta da Gustav Klimt.
            </p>
            <h2 class="caps">...il primo comune "libero" d'Italia </h2>
            <p>
Lazise sul Garda viene considerato il primo comune “libero” d’Italia, la comunità nel 983 d.C. chiese a Ottone II, Imperatore del Sacro Romano Impero,
 la possibilità di proteggere e difendere la città fortificando il “castrum” (era l’accampamento dell’esercito romano di forma rettangolare attorno al quale veniva scavato un fossato) e ottenne il diritto di transito e di pesca e la completa autonomia per le questioni civili, economiche e religiose. 
Queste autonomie non erano concesse a tutti tanto che venne considerato come il primo comune “libero” d’Italia.
            </p>            
            <h2 class="caps">...il produttore più a Nord</h2>
            <p>
                Il Lago di Garda rientra tra i produttori più a Nord per alcune categorie di prodotti… 
                Da sempre Limone sul Garda è stata considerata come il produttore di limoni più a Nord del mondo. I prodotti erano esportati in Germania, Polonia e Russia garantendo ovviamente un grande profitto e posti di lavoro. L’attività cessò successivamente al secondo conflitto mondiale,
                 ma ancora oggi è possibile visitare la limonaia del Castello per vedere con i propri occhi quanto questo territorio sia effettivamente prospero per lo sviluppo di questo tipo di pianta.
Riva del Garda oggi è considerato il produttore più a Nord di ulivi e di olio di altissima qualità che ha ottenuto importanti premi in fiere internazionali.
            </p>

		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
						 <br>
						 <br>
        </div>
<!-- fine div contenitore-->
<!-- parte inferiore -->


	<div >
   		<p> Lago di Garda - Copyright 2022 © </p>

	</div>

	<div >
		<img src="../../foto/footer.jpg" class="cento" alt="">
	</div>

<!-- FINE FINALE -->





</body>
</html>
