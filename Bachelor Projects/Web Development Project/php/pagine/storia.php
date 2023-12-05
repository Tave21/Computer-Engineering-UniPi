
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
			<h1 id="sps" > UN SALTO NELLA STORIA</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->


        <div  class="copre">
        
  <br>
            <h2 class="caps" id ="vitt" style="margin-left: 41%;">Vittoriale degli italiani </h2>
            <p class="jus">
Il Vittoriale degli Italiani è un complesso di edifici, vie, piazze, un teatro all'aperto, giardini e corsi d'acqua eretto tra il 1921 e il 1938 a Gardone Riviera,
 sulla sponda bresciana del lago di Garda. Committente del complesso fu il poeta e romanziere Gabriele d'Annunzio che vi è qui sepolto e che ne affidò il progetto all'architetto Giancarlo Maroni in modo che il luogo rappresentasse la memoria della "vita inimitabile" del poeta-soldato
  e delle imprese dei soldati italiani durante la prima guerra mondiale.
  Spesso con Vittoriale ci si riferisce alla sola abitazione di d'Annunzio, situata all'interno del complesso, che invece è la parte denominata Prioria.
Il Vittoriale oggi è un monumento aperto al pubblico e visitato ogni anno da circa 210.000 persone[1].
«Ho trovato qui sul Garda una vecchia villa appartenuta al defunto dottor Thode. È piena di bei libri... Il giardino è dolce, con le sue pergole e le sue terrazze in declivio. E la luce calda mi fa sospirare verso quella di Roma. Rimarrò qui qualche mese, per licenziare finalmente il Notturno» 
scrive d'Annunzio alla moglie Maria in una lettera del febbraio del 1921, cioè pochi giorni dopo il suo arrivo a Gardone; nelle intenzioni del poeta il soggiorno gardesano doveva durare dunque solo poche settimane per completare la stesura del suo ultimo romanzo, mentre oggi si sa che quella gardonese sarebbe diventata la sua ultima e definitiva dimora.
            </p>
  <br>
            <h2 class="caps" id="cat" style='margin-left: 43%;'>Grotte di Catullo </h2>
            <p class="jus">
Sirmione è posta lungo la penisola omonima che si protende all'interno del lago di Garda per circa quattro km e che divide in due parti la riva lacuale meridionale. Parte del territorio comunale si estende ad est rispetto alla penisola per includere quella di Punta Grò.

In quest'oasi di pura bellezza sorge uno dei siti più belli rispetto alle ville romane dell'Italia settentrionale, e cioè le cosiddette "Grotte di Catullo".
Per Grotte di Catullo si intendono i resti di una villa romana edificata tra la fine del I sec. a.c. e il I sec. d.c. a Sirmione,  sulla riva meridionale del Lago di Garda.

Essa godeva di una posizione eccezionale, sulla punta della penisola di Sirmione, dominante dall'alto dello sperone roccioso l'intero bacino del Lago.

La denominazione di "Grotte di Catullo" risale al Quattrocento, quando la riscoperta delle liriche di Catullo, fra cui il Carme 31, in cui il poeta descrive il suo ritorno nell'amata casa di Sirmione, fece associare i grandiosi resti ancora visibili anche se quasi tutti interrati e coperti da vegetazione.

Per questa ragione vennero denominate grotte. Il primo ad attribuire la villa a Gaio Valerio Catullo fu, nel 1483, il giovane studioso Marin Sanudo il giovane. Oggi si giudica del tutto improbabile che la villa sia quella di Catullo visto che sembra sia stata edificata successivamente alla sua morte.

Vero è però che una struttura del I sec. a.c. giace sotto quella riedificata nel I sec. d.c. Essendo Catullo del I sec. a.c. (morto nel 56 o 54 a.c.) avrebbe ben potuto essere il proprietario del primo edificio.

I resti  attualmente conservati si trovano oggi su livelli diversi. Del settore settentrionale ad esempio sono rimaste solo le grandi sostruzioni, mentre nulla è conservato dei vani residenziali, crollati già in epoca antica.
            </p>
  <br>
            <h2 class="caps" id="sirm" style='margin-left: 39%;'>Castello Scaligero Sirmione </h2>
            <p class="jus">
La famiglia Della Scala governò su Verona e il suo territorio dal 1259 al 1387: le caratteristiche architettoniche del castello sono riconducibili al periodo di Cansignorio e di Antonio II Della Scala, ossia al 1360-1380 circa, e la rocca fu presumibilmente costruita nell’arco di alcuni anni, ma secondo un progetto unitario, che la dotava anche di un porto fortificato, la darsena, dove accogliere la flotta.

Dal XVI secolo l’importanza di Sirmione come postazione difensiva diminuì a favore della fortezza di Peschiera del Garda. Nei secoli successivi, in epoca napoleonica e poi sotto la dominazione austriaca, il castello fu utilizzato come deposito di armi e vettovaglie e alloggio per le truppe. Successivamente, ospitò gli uffici comunali, l’ufficio postale, l’alloggio dei Carabinieri e un piccolo carcere.

L’aspetto attuale fu definito nel corso dei restauri eseguiti dopo il 1919.
            </p>
  <br>
            <h2 class="caps" id="malce" style='margin-left: 39%;'>Castello Scaligero Malcesine </h2>
            <p class="jus">
La Rocca di Malcesine risalirebbe agli ultimi secoli del primo millennio a.C. ma risulta più attendibile la notizia che un castello sia stato costruito dai Longobardi, verso la metà del primo millennio d.C.
Il castello venne distrutto dai Franchi nel 590. Fu da loro stessi riedificato e ospitò nel ‘806 il re Pipino, giunto a Malcesine per visitare i Ss. Benigno e Caro. Dopo le invasioni degli Ungari, entrò a far parte dei feudi vescovili veronesi.
Nel 1277 divenne dominio di Alberto della Scala. Rimase sotto il casato fino al 1387.
Interventi risalenti a questo periodo diedero origine all’attuale denominazione: “Castello Scaligero”.
I Visconti di Milano lo occuparono dal 1387 al 1403.
La Repubblica di Venezia lo incorporò nel 1405. L’Impero lo riconquistò nel 1506. Ritornò alla Repubblica dal 1516 al 1797. Poi passò ai Francesi.
Nel 1798 ai Francesi subentrarono gli Austriaci, i quali eseguirono consistenti lavori di consolidamento all’interno del Castello e lì rimasero fino al 1866.
Da quell’anno seguì le sorti del Veneto.
Il 22 agosto del 1902 venne dichiarato Monumento Nazionale.
            </p>      
            
<hr>
            <h1 class="qa" id="altre">Altre località storiche</h1>
           <br> 
            <h2 class="caps" style="margin-left: 45%;">Villa Alba</h2>
            <p class="jus">
                Posizionata sulla sponda occidentale del lago di Garda,
                 Villa Alba nasce a Gardone Riviera una delle località più amate della zona. 
                 Gardone Riviera, tra i borghi più belli della Lombardia, diventa famoso ai turisti 
                 soprattutto per il Vittoriale degli Italiani costruito nel 1921 per volere del poeta
                  Gabriele D’Annunzio ma in realtà rivela numerosi patrimoni storici e artistici che
                   meritano una visita.
                   Villa Alba è stata costruita ad inizio del ‘900 ma era conosciuta con il nome 
                   di “Villa Ruhland”. Il nome originale tradotto significa “pace nel paesaggio” e calza a 
                   pennello considerando l’atmosfera che si avverte. Villa Alba è circondata da un giardino 
                   lussureggiante e vanta una facciata di incredibile pregio che richiama il Partenone grazie 
                   alla scelta del colonnato e del frontone decorato con bassorilievi. Il tempio laico della bellezza in
                    stile neoclassico oggi è utilizzato per eventi, matrimoni, mostre e rassegne d’arte.
            </p>
            <h2 class="caps" style="margin-left: 42%;">Castello di Moniga</h2>
            <p class="jus">
              Il castello si trova nella parte occidentale dell'abitato, su una leggera altura coltivata a vigneto; non è, come altre costruzioni simili, sul ciglio di una scarpata, ma sulla strada che collega alcuni dei castelli di cui abbiamo parlato.
              Ed è proprio la sua posizione ben poco strategica, grazie alla quale non ha mai subito assalti e conquiste significativi, che gli ha garantito una così buona conservazione, preservandolo da profonde trasformazioni architettoniche.
              Fu costruito, come gli altri, per far fronte alle invasioni ungare del X secolo; poi, ormai diroccato, per un certo periodo il castello venne abbandonato.
              Più tardi iniziarono a stabilirvisi pastori e contadini che possedevano terre nei dintorni: così, da questi accampamenti improvvisati, nacque l'idea di abitare stabilmente il castello.
              I materiali impiegati nella ricostruzione fanno pensare che le murature siano del XIV e del XV secolo, periodo al quale si fanno risalire tutte le strutture oggi visibili. 
            </p>
            <h2 class="caps" style="margin-left: 36%;">Castello di Desenzano del Garda</h2>
            <p class="jus">
               Il castello che domina la città, sorto forse su un castrum romano a difesa delle invasioni barbariche, fu ricostruito in epoca comunale e rafforzato nel XV secolo, quando racchiudeva 120 case ed una chiesa dedicata a S. Ambrogio.
                Alla fine del Quattrocento il castello, che ebbe origine nell'Alto Medioevo e probabilmente sorse sulle fondamenta di un castrum romano, fu ampliato nella parte sud, ma non divenne mai una fortezza militare, anche se l'ingrandimento fu fatto per ospitare una guarnigione. Continuò ad assolvere soprattutto la funzione di rifugio per la popolazione, la torre del Castello di Desenzano.
                Nell'interno del castello continuarono ad esservi abitazioni di privati cittadini pronti ad accogliere, in caso di pericolo, coloro che abitavano fuori le mura di esso.
                In seguito il castello perse via via la sua funzione di rifugio, continuando ad essere abitato da famiglie, anche se il suo degrado, per quasi tutto l'Ottocento, procedette sempre più grave. Nel castello v'era pure una chiesa, la chiesa di S. Ambrogio, che fu utilizzata come casa privata.
                La pianta del castello è quella di un rettangolo irregolare, con la torre che si innalza all'ingresso, sul lato nord, a protezione del ponte levatoio, di cui si conservano le feritoie per le catene. E' una torre massiccia dotata di finestre solo nella parte superiore.
                Nel 1882 il castello fu adibito a caserma, prima sede di una guarnigione di fanteria, poi di bersaglieri e infine di alpini dagli anni trenta fino al 1943.
                Dell'antico castello rimangono alcuni tratti di cortine murarie con merli sgretolati fra le quattro torri angolari mozze, ad eccezione di quella sullo spigolo a nord-est che, fino al 1940, funzionò come specola. Dalla sua terrazza si gode uno dei più bei panorami del Garda. 
            </p>
        <hr>
          <br>
        <h2  class="caps" id="legg" style="margin-left: 41%;">Tra Storia e Leggenda</h2>
        <p class="jus">
        Anche il Castello Scaligero di Sirmione come molti altri manieri ha una sua leggenda che prende le sembianze di un fantasma che si aggira tra le sale del castello nelle notti di tempesta.
        La leggenda narra che un giovane di nome Ebengardo, che viveva tranquillo nel castello con la sua amata sposa Arice, durante una terribile notte di pioggia e vento sferzante, ospitò tra le mura della Rocca di Sirmione un cavaliere che si era presentato come Elaberto, Marchese del Feltrino.
        Il marchese completamente rapito dalla bellezza di Arice e deciso a possederla a ogni costo, nel corso della notte si introdusse nelle stanze della dama per portare a termine il suo terribile piano.
        Alte furono le grida della donna che svegliarono il suo sposo che si precipitò nella stanza, trovandola troppo tardi. Era stata strappata alla vita dal maligno pugnale di Elaberto.
        Furente e accecato dalla rabbia Ebenegardo si lanciò verso l’assassino ma nella violenza della colluttazione rimase ucciso dal suo stesso pugnale.
        Si dice che disperato per non aver salvato la sua amata e neppure vendicato la di lei morte, si aggiri come uno spettro tra le stanze del castello, condannato da un destino infausto a non ricongiungersi con la sua Arice neppure dopo la morte.

        </p>
		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
						 <br>
						 <br>
        </div>

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