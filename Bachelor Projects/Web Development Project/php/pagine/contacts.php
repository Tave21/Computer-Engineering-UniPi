
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


?><style>
	input[type=submit] {
    padding:5px 15px; 
    background:#ccc; 
    border:0 none;
    cursor:pointer;
    -webkit-border-radius: 5px;
    border-radius: 5px; 
}
</style>
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
			<h1 id="sps" class="home" > CONTATTI & ORARI</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->



<!-- contenitore-->	
<div style="background-color: antiquewhite;">
		<div  class="riga">
			<div class ="stacca">
				<br>
	   		<h2 class="caps" >Vittoriale degli Italiani</h2>
				<p>
				<strong>Indirizzo</strong>: Via del Vittoriale 12 25083  Gardone Riviera (BS) <br>
				<strong>Orario</strong>: <br>
				Aperto tutti i giorni dalle 9 alle 17 <br>	
		
				<strong> Telefono</strong>: 0365 296511

				</p>
				<br>
				<br>
				<br>
				<br>
				<br>
				<br>
				<br>
				
			</div>
			<div class ="stacca">
				<h2 class="caps" >Castello Scaligero Sirmione</h2>
				<p>
				<strong> Indirizzo</strong>: P.za Castello, 34, 25019 Sirmione BS <br>
				<strong>Orario:</strong> <br>
				<ol>
					<li>  sabato	9-18:30  </li>
					<li>  domenica 9-12:50  </li>
					<li>  lunedì	Chiuso </li>
					<li>  martedì	9-18:30  </li>
					<li>  mercoledì 9-18:30  </li>
					<li>  giovedì	9-18:30  </li>
					<li>  venerdì	9-18:30  </li>

				</ol>
		
				<strong> Telefono</strong>: 030 916468
				</p>
			</div>
		</div>

		<div class="riga">

			<div class ="stacca">

	 		<h2 class="caps" >Castello Scaligero Malcesine</h2>
	   		<p>
		   		<strong> Indirizzo</strong>: Via Castello, 37018 Malcesine VR <br>
		   		<strong>Orario:</strong> <br>
		   		ORARIO APERTURA 1 MARZO - 31 OTTOBRE 2021 (Novembre-Febbraio CHIUSO) <br>
		   		da lunedì a domeinca dalle ore 9.30 alle ore 19.30* <br>
		   		(*L’ingresso è consentito fino alle ore 19:00) <br>
		   		<strong> Telefono</strong>: 045 657 0333

	   		</p>
			</div>

			<div class ="stacca">
	 		<h2 class="caps" >Grotte di Catullo </h2>
				<p>
					<strong> Indirizzo</strong>: Piazzale Orti Manara 4 25019 Sirmione (BS) <br>
					<strong>Orario:</strong> <br>
					Da martedì a sabato: h 8.30-17.00 <br>
					Domenica: h 8.30-14.00	<br>
					Giorni di chiusura: il 1° gennaio, il 1° maggio e il 25 dicembre, salvo diverse indicazioni. <br>	
					<strong> Telefono</strong>: 030 916157 

	


				</p>
			</div>
		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
						 <br>
						 <br>
 		</div>
</div>
			<!-- fine primo blocco -->       
    

 
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
