
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
	<div>
		
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
			<h1 id="sps" > LAGO DI GARDA SULLA MAPPA</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>

	<div class="mappa">
		<br>
	  <div class="se">
		<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d356930.81187449826!2d10.413651402028115!3d45.66042080939859!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x4781eca8aec020b9%3A0x91dcf07c1c969bb8!2sLago%20di%20Garda!5e0!3m2!1sit!2sit!4v1646500299896!5m2!1sit!2sit" width="800" height="600" style="border:0;" allowfullscreen="" loading="lazy"></iframe>
	  </div>
	  <br>
   	  <div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button>
	  </div>
	  <br>
	</div>



<!-- parte inferiore -->
<br>
	<div >

   		<p> Lago di Garda - Copyright 2022 © </p>

	</div>

	<div >
		<img src="../../foto/footer.jpg" class="cento" alt="">
	</div>

<!-- FINE FINALE -->



</body>
</html>
