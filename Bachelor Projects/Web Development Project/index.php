
<!DOCTYPE html>

<html lang="it">


<head>
      <meta charset="utf-8">
      <meta name="viewport" content="width = device-width">
    <title >Lago di Garda & i suoi Siti storici </title>
	<link rel="stylesheet" type="text/css" href="css/style.css" />
	<script src="javascript/funz.js" ></script>
</head>

<?php
	session_start(); 
?>

<body onload="endler()">


	<!-- inizio top  -->
	<div >

		<!--  Inizio Menu navigazione -->
		<div>
			<ul>
     	  		<li><a href="index.php">Home</a></li>
        		<li><a href="php/pagine/reservation.php">Prenotazione Visite</a></li>
        		<li>
            		<a href="#">Social Newtork&#9662;</a>
            		<ul class="dropdown">
                		<li><a href="https://www.facebook.com/LagodiGardaItaly">Facebook</a></li>
                		<li><a href="https://twitter.com/LagoGardaPoint">Twitter</a></li>
            		</ul>
        		</li>
        		<li><a href="php/pagine/contacts.php">Contatti & Orari</a></li>
				<li><a href="">Media&#9662;</a>
            		<ul class="dropdown">
							<li><a href="php/pagine/foto.php">Foto Gallery</a></li>
							<li><a href="https://www.youtube.com/watch?v=IlYEwjc-o7c&list=PL944DF0F01F762B24">Video Gallery</a></li>
            		</ul>
				</li>
				<li>
				
				<?php
  				if(isset($_SESSION['logged'])) {
					  echo '<a href ="php/pagine/areaPersonale.php"> Area Personale</a>';
				  } else {
					  echo '<a href ="php/login/index2.php"> Accedi o Registrati </a>';
				  }  
				?>

				</li>	
				<li>
            		<a href="#">Menu rapido&#9662;</a>
            		<ul class="dropdown">
                            <li><a href="php/pagine/recensioni.php">Recensioni</a></li>
                            <li><a href="php/pagine/storia.php">Storia</a></li>
							<li><a href="php/pagine/curiosita.php">Curiosità</a></li>
							<li><a href="php/pagine/newsletter.php">Newsletter</a></li>
							<li><a href="php/pagine/mappe.php">Maps</a></li>
            		</ul>
        		</li>
						<?php
  				if(isset($_SESSION['logged'])) {
					  echo '
				<li>
			      <form name="logout_form" id="logout_form" method="post" action="php/login/logout.php" enctype="multipart/form-data">
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
			<h1 id="tito" style="text-align: center;" > LAGO DI GARDA & I SUOI SITI STORICI </h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->

<!-- contenitore slideshow -->
		<div class="contenitoreslide">

  		<div class="slide fade">
    		<div class="numfoto">1 / 4</div>
    		<img src="foto/vittoriale.jpg" style="width:100%" alt="" >
    		<div class="text">
				<h1 class="c" > Vittoriale degli Italiani</h1>
				<p class="nero">cittadella monumentale costruita da Gabriele d’Annunzio a Gardone Riviera </p>
			</div>
  		</div>

  		<div class="slide fade">
    		<div class="numfoto">2 / 4</div>
    		<img src="foto/malcesine.jpg" style="width:100%" alt="">
    		<div class="text"> 
				<h1 class="c" >Castello Scaligero Malcesine</h1>
				<p class="nero"> Prenota una visita nella storia</p>
			
			</div>
  		</div>

  		<div class="slide fade">
    		<div class="numfoto">3 / 4</div>
    		<img src="foto/catullo.jpg" style="width:100%" alt="">
    		<div class="text">
				<h1 class="c"> Grotte di Catullo</h1>
				<p class="nero">uno dei punti più suggestivi e proprio in mezzo al lago più grande d’Italia  </p>
			</div>
  		</div>

		<div class="slide fade">
    		<div class="numfoto">4 / 4</div>
    		<img src="foto/sirmione.png" style="width:100%" alt="">
    		<div class="text">
				<h1 class="c">Castello Scaligero Sirmione</h1>
				<p class="nero"> una delle più spettacolari e meglio conservate rocche scaligere del Garda</p>
			</div>
  		</div>

  		<!-- bottone avanti e indietro-->
  		<a class="prev" onclick="slideavanti(-1)">&#10094;</a>
  		<a class="next" onclick="slideavanti(1)">&#10095;</a>
		</div>
		<br>

		<!-- Cerchietti in basso-->
		<div style="text-align:center">
  		<span class="pallino" onclick="questaslide(1)"></span>
  		<span class="pallino" onclick="questaslide(2)"></span>
  		<span class="pallino" onclick="questaslide(3)"></span>
  		<span class="pallino" onclick="questaslide(4)"></span>
		</div>	
	<!-- fine slide show -->	

<!-- contenitore-->	
<div  style="background-color: antiquewhite;">
	<!-- Prima riga-->
		<div class="riga">

                <!-- seconda colonna-->
				<div class="prenotazioni">
					
					<h2 >Evento in copertina</h2>
					<p class="sub-headings">in programma..</p>
    				<img src="foto/eventocatu.jpg" style="width:100%" alt="" >

				</div>
					
				<!-- fine seconda colonna-->
                
    
    			<!-- terza colonna-->
				<div class="prenotazioni">
					
					<h2 class="first-headings">Curiosità di un lago storico</h2>
					
					<p class="sub-headings"> Scopri di più sul lago
	        		</p>
					
					<p>
						<a href="php/pagine/curiosita.php"><img src="foto/curiosita.jpg" width="350" height="390" alt="" /></a>
                       
				</div>
				<!-- fine terza colonna-->     
	    </div>
			<!-- fine primo blocco -->       
    

 <br />
 <hr>
			<!-- inizio seconda riga-->
			<div class="riga">
				<h1 class="tit">Storia</h1>
				<!-- prima colonna -->
				<div class="storia">
					<h2>Castello Scaligero Sirmione</h2>
          			<a  href="php/pagine/storia.php#sirm"><img class="imImage" src="foto/sirmionestoria.jpg" alt="" ></a>
          		</div>
          
				<!-- fine prima colonna-->
  
				<!-- inizio seconda colonna-->
	  				<div class="storia">
	    				<h2 class="alto">Grotte di Catullo</h2>
						<a  href="php/pagine/storia.php#cat"><img class="imImage" src="foto/grottestoria.jpg"alt="" ></a>
	  				</div>
				<!-- fine seconda colonna -->
				
				<!-- inizio terza colonna-->
	  			<div class="storia">
	    			<h2 class="alto" >Vittoriale degli Italiani</h2>
					<a  href="php/pagine/storia.php#vitt"><img class="imImage" src="foto/vittorialestoria.jpg"alt=""  ></a>
				</div>
				<!-- fine terza colonna-->
				
				<!-- inizio quarta colonna-->
							<div class="storia">
				  			<h2>Castello Scaligero Malcesine</h2>
					 		<a  href="php/pagine/storia.php#malce"><img class="imImage" src="foto/malcesinestoria.jpg" alt="" ></a>
							</div>
       			<!-- fine quarta colonna -->       

			</div> 
			<br>
<hr>

	<!--  LEGGENDA E CURIOSITA -->
		<div >
		<table class="lll">
				      		<tr >
				        		<td style="background-color: antiquewhite;" ><h2>Tra storia e leggenda</h2></td>
				        		<td style="background-color: antiquewhite;" ><h2>Altre località storiche sul Garda</h2></td>
			          		</tr>
				      
                      		<tr>
				        		<td style="background-color: antiquewhite;"><a href="php/pagine/storia.php#legg"><img src="foto/leggenda.jpg" alt="" width="420" height="180" onmouseover= "opacity"  /></a></td>
				        		<td style="background-color: antiquewhite;" ><a href="php/pagine/storia.php#altre"><img src="foto/guida.png" alt="" width="420" height="180" /></a></td>
                      		</tr>
      		</table>
		</div>
		<br>
	<hr>

		<!-- terza riga-->
		<div class="riga">
			
			<!-- prima colonna-->
	  			<div class="fine">
	    			<h2>Newsletter</h2>
				<p class="sub-headings">Tieniti aggiornato! </p>
				<a  href="php/pagine/newsletter.php"><img class="imImage" height="180" width="200" src="foto/news.jpg" alt=""></a>
	  			</div>
			<!-- fine prima colonna-->
				
			<!-- seconda colonna-->
	  			<div class="fine">
	    			<h2>Maps</h2>
					<p class="sub-headings">Dove siamo</p>
					<a  href="php/pagine/mappe.php"><img class="imImage" height="180" width="180" src="foto/maps.jpg"alt="" ></a>
				</div>
			<!-- fine seconda colonna-->

			<!-- terza colonna -->
				<div class="fine">
		  			<h2>Foto Gallery</h2>
					<p class="sub-headings">
		  			Guarda le foto del lago dei turisti!</p>
    			<a href="php/pagine/foto.php"><img src="foto/foto.jpg" alt="" width="168" height="180" /></a>
  				</div> 
			<!-- fine terza colonna-->

			<!-- quarta colonna -->
				<div class="fine">
		  			<h2>Recensioni</h2>
					<p class="sub-headings">
		  			Leggi esperienze sui siti storici</p>
    			<a href="php/pagine/recensioni.php"><img src="foto/recensioni.jpg" alt="" width="168" height="180" /></a>
  				</div> 
			<!-- fine quarta colonna-->
				<br>
				<br>
  		</div> 
		 				 <div class="home"><br> <br><button id="home" onclick="document.location.href='index.php';">Home Page</button> </div>
						  <br>
						  <br>

</div>
<!-- fine div contenitore-->
<!-- parte inferiore -->
      
	  

	<div >
   		<p> Lago di Garda - Copyright 2022 © </p>

	</div>

	<div >
		<img src="foto/footer.jpg" class="cento" alt="">
	</div>

<!-- FINE FINALE -->




</body>
</html>
