-- 1

drop procedure if exists AggiungiModalita;
delimiter $$
create procedure AggiungiModalita(in Nome_ varchar(30), in Stanze_ varchar(250))
begin
	declare indice int default 0;
    declare flag int default 0;
    declare str varchar(250) default '';
	declare counter int default 1;
    declare controllo int default 0;
    
    if not exists (select 1 from programmi where NomeProgramma = Nome_)then
		signal sqlstate '45000'
        set message_text = "Programma non esistente";
    end if;
    
    drop table if exists Stanze;
	create temporary table Stanze(
		Stanza varchar(10), 
        primary key(`Stanza`)
	);

	set str = Stanze_;
    
    giro: loop
		select locate("-",str) into indice; #restituisce la posizione del delimitatore
        insert into Stanze
		select substring_index(str,'-',1); #prende la prima stanza prima del delimitatore
		select substring(str,indice+1) into str; #cancella la prima stanza e il delimitatore
        if char_length(str) = 4 then #ripete finche' la stringa non e' vuota
			if flag = 0 then
				set flag = flag + 1; #serve per fare un ciclo un piu'
                set counter = counter + 1;
                iterate giro;
			else
				leave giro;
			end if;
		end if;
        set counter = counter + 1;
    end loop giro;
    
    set controllo = (
    select count(distinct stanza)
    from Stanze natural left outer join (Dispositivi D inner join programmi P on D.Codice = P.Dispositivo)
    where P.NomeProgramma = Nome_);
    if controllo <> counter then
		signal sqlstate '45000'
        set message_text = "Programma non esistente per alcuni dispositivi nelle stanze indicate";
	end if;
    
    
    insert into modalita
	(Nome,Stanza) values (Nome_,Stanze_);
    
end $$
delimiter ;

call AggiungiModalita('Mattina','2001-2002-3003-5010-0000');

select *
from modalita;
-- 2
drop procedure if exists SalvataggioModalitaInRegistri;
delimiter $$
create procedure SalvataggioModalitaInRegistri(in Nome_ varchar(250), in DataInizio_ date, in DataFine_ date, in Utente_ varchar(10))#str
begin
	declare indice int default 0;
    declare flag int default 0;
    declare NomeProgramma__ varchar(10);
    declare Dispositivo__ int;
    declare str varchar(250) default '';
    
    if not exists (select 1 from modalita where nome = Nome_) then
		signal sqlstate '45000'
		set message_text = 'Modalità non presente';
	end if;
    
	drop table if exists Stanze;
	create temporary table Stanze(
		Stanza varchar(10), 
        primary key(`Stanza`)
	);
    
    select Stanza into str
    from Modalita
    where Nome = Nome_;
    
    giro: loop
		select locate("-",str) into indice; #restituisce la posizione del delimitatore
        insert into Stanze
		select substring_index(str,'-',1); #prende la prima stanza prima del delimitatore
		select substring(str,indice+1) into str; #cancella la prima stanza e il delimitatore
        if char_length(str) = 4 then #ripete finche' la stringa non e' vuota
			if flag = 0 then
				set flag = flag + 1; #serve per fare un ciclo un piu'
                iterate giro;
			else
				leave giro;
			end if;
		end if;
    end loop giro;
    begin
		declare finito int default 0;
		declare scorr cursor for
		select NomeProgramma, Dispositivo
		from Stanze natural join Dispositivi C inner join Programmi P on C.Codice = P.Dispositivo
		where NomeProgramma = Nome_;
        
        declare continue handler
        for not found set finito = 1;
        
        open scorr;
        
        giro: loop
			fetch scorr into NomeProgramma__, Dispositivo__;
            if finito = 1 then
				leave giro;
			end if;
            call SalvataggioProgrammaInRegistri(NomeProgramma__, Dispositivo__, DataInizio_, DataFine_, Utente_);
        end loop giro;
        
        close scorr;
    end;
end $$
delimiter ;

call SalvataggioModalitaInRegistri("Mattina","2022-02-01", "2023-02-01","malstead1");


#calcolo del consumo medio e controllo range

drop procedure if exists SalvataggioProgrammaInRegistri;
delimiter $$
create procedure SalvataggioProgrammaInRegistri(in NomeProgramma_ varchar(250), in Dispositivo_ int, in DataInizio_ date, in DataFine_ date, in Utente_ varchar(10))
begin
	#controlli
    declare flag int default 0;
    if not exists (select 1 from programmi where NomeProgramma = NomeProgramma_) then
		#signal sqlstate '45000'
		select 'Programma non presente';
        set flag = 1;
	end if;
    if not exists (select 1 from Dispositivi where codice = Dispositivo_) then
		#signal sqlstate '45000'
		select 'Dispositivo non presente';
        set flag = 1;
	end if;
    if not exists (select 1 from utente where nomeutente = Utente_) then
		#signal sqlstate '45000'
		select 'Utente non presente';
        set flag = 1;
	end if;
    if exists (select 1 from registri where Dispositivo = Dispositivo_ and DataInizio_ < DataFine and DataFine_ > DataInizio) then
		#signal sqlstate '45000'
		select concat('Già presente programma in esecuzione nel dispositivo: ', (select nome from Dispositivi where Codice = Dispositivo_)); #riguarda la documentazione update della data TODO
        set flag = 1;
	end if;
    if flag = 0 then
		insert into Registri
		values(NomeProgramma_, Dispositivo_, DataInizio_, DataFine_, current_date, Utente_, (select ConsumoMedio from programmi where NomeProgramma = NomeProgramma_ and Dispositivo = Dispositivo_));
        
		if exists (select 1 from elementoilluminazione where Codice = Dispositivo_) then
			begin
				declare intensita_ int;
                declare colore_	varchar(10);
                declare temperatura_ int;
                select Potenza, Colore, Temperatura into intensita_, Colore_, temperatura_
                from programmi
                where NomeProgramma = NomeProgramma_ and dispositivo = Dispositivo_;
                insert into luci
				values(NomeProgramma_, Dispositivo_, DataInizio_,DataFine_,(intensita_),(colore_),(temperatura_));
            end ;
		elseif exists (select 1 from elementocondizionamento where codice = Dispositivo_) then
			begin
				declare TemperaturaFinale_ int;
                declare Umidita_ int;
                declare intensita_ int;
                select Temperatura, Umidita, Potenza into TemperaturaFinale_, Umidita_, Intensita_
                from programmi
                where dispositivo = Dispositivo_ and NomeProgramma = NomeProgramma_;
                insert into condizionamento
				values(NomeProgramma_, Dispositivo_, DataInizio_,DataFine_,(FLOOR(RAND()*(20-10+1)+10)),(TemperaturaFinale_),(Umidita_),(intensita_));#temperatura iniziale calcolata a caso
            end ;
		else 
			insert into dispositivoGenerico
			values(NomeProgramma_, Dispositivo_, DataInizio_,DataFine_,((select potenza from programmi where Dispositivo = Dispositivo_ and NomeProgramma = NomeProgramma_) > 0));
		end if;
        
    end if;
end $$
delimiter ;
call SalvataggioProgrammaInRegistri("Arancione",2,"2022-01-01","2022-03-02", "malstead1");





-- 3

drop trigger if exists AggiornamentoEnergiaConsumata;
delimiter $$
create trigger AggiornamentoEnergiaConsumata
after insert on Registri
for each row
begin
	declare fasciaOraria_ varchar(20) default '';
    declare ora int default date_format(new.dataInizio, '%H');
    case ora
		when ora then set fasciaOraria_ = 'Mattina';
		when ora then set fasciaOraria_ = 'Pomeriggio';
		when ora then set fasciaOraria_ = 'Sera';
		when ora then set fasciaOraria_ = 'Notte';
    end case;
    insert into EnergiaConsumata
	values(new.Dispositivo, new.dataInizio,new.datafine,new.consumomedio * greatest(0,timestampdiff(hour, new.datainizio,new.datafine)), fasciaOraria_,new.nomeimpostazione, new.utente);
end $$
delimiter ;

insert into registri
values('elimina',10000,'2020-10-15 11:13:17','2022-11-02 15:13:17','2020-11-14 11:13:17','banana',500);

-- 4

-- aggiunta in archivio energia
drop event if exists AggiornamentoArchivioEnergia;
delimiter $$
create event AggiornamentoArchivioEnergia
on schedule every 1 day
starts '2021-09-19 1:00:00'
do
begin
	declare consumoTotale bigint default 0;
    declare finito int default 0;
    declare OraStart datetime;
    declare OraEnd datetime;
    declare CMedio int;
    declare indice int default 0;
    declare sumEolico int default 0;
    declare sumSolare int default 0;
    declare auxFasciaOraria varchar(20);
    declare Base timestamp default current_timestamp - interval 1 day - interval 1 hour;#la mezzanotte del giorno prima
    declare scorr cursor for
    select OraAccensione, OraSpegnimento, Consumo
	from energiaconsumata
	where OraAccensione < current_timestamp - interval 1 hour and OraSpegnimento > current_timestamp - interval 1 hour - interval 1 day;
    
    declare continue handler
    for not found set finito = 1;
    
    drop table if exists totaleProdotto;
    create temporary table totaleProdotto(
		Fonte varchar(10),
        Data date,
        FasciaOraria varchar(10),
        tot int,
        primary key(`Fonte`, `Data`, `FasciaOraria`)
	);
    
    insert into totaleProdotto
    select distinct Fonte,Data, FasciaOraria, sum(Quantita) over(partition by FasciaOraria, Data, Fonte) as tot
	from energiaprodotta;
    
    open scorr;
    
    while indice < 24 do
		giro: loop
			fetch scorr into OraStart, OraEnd, CMedio;   
			if finito then
				close scorr;
                open scorr;
                set finito = 0;
				#insert
                case indice
                when 0 then
					set auxFasciaOraria = 'notte';
					set sumEolico = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Eolico');
					set sumSolare = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Solare');
                when 6 then
					set auxFasciaOraria = 'mattina';
					set sumEolico = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Eolico');
					set sumSolare = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Solare');
				when 12 then
					set auxFasciaOraria = 'pomeriggio';
                    set sumEolico = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Eolico');
					set sumSolare = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Solare');
                when 18 then
					set auxFasciaOraria = 'sera';
                    set sumEolico = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Eolico');
					set sumSolare = (select tot from totaleProdotto where FasciaOraria = auxFasciaOraria and Data = current_date - interval 1 day and Fonte = 'Solare');/*2021-09-18'*/
                end case;
                set indice = indice + 6;
                insert into archivioEnergia
                values(current_date - interval 1 day,auxFasciaOraria, sumEolico, sumSolare, consumoTotale);
				leave giro;
			end if;
			set consumoTotale = consumoTotale + CMedio * greatest((6 - (greatest(0,timestampdiff(hour,base + interval indice hour,OraStart)) + greatest(0,timestampdiff(hour,OraEnd,base + interval (indice + 6) hour)))),0);
		end loop;
        set consumoTotale = 0;
	end while;
    
    close scorr;
end $$
delimiter ;

-- 4

#pulizia energia consumata e prodotta
drop event if exists CleaningEnergiaConsumataEprodotta;
delimiter $$
create event CleaningEnergiaConsumataEprodotta
on schedule every 1 day
starts '2021-09-18 1:00:00'
do
begin
	delete from energiaconsumata
    where OraSpegnimento = current_date - interval 1 day;
    
    delete from energiaProdotta
    where data = current_date - interval 1 day;
end $$;
delimiter ;

-- 5
#analisi prod per sugg
drop event if exists GenerazioneSuggerimenti;
delimiter $$
create event GenerazioneSuggerimenti
on schedule every 2 hour
starts '2021-09-18 00:00:00'
do
begin
	declare energiaConsumata int default 0;
    declare energiaProdotta int default 0;
    
    select sum(Consumo) into energiaConsumata
    from energiaconsumata
    where current_timestamp() between OraAccensione and OraSpegnimento;
    
    select Quantita into energiaProdotta
	from energiaprodotta
	where Orario < date_format(current_time(),'%H:%m') and Fonte = 'Solare'
	order by Orario desc
	limit 1;

	set energiaProdotta = energiaprodotta + (
		select Quantita
		from energiaprodotta
		where Orario < date_format(current_time(),'%H:%m') and Fonte = 'Eolico'
		order by Orario desc
		limit 1
    );
    
    if (energiaprodotta * 1000 - energiaconsumata) > 1000 then 
        insert into suggerimenti
        values(
				(
					select NomeUtente
					from account
					order by rand()
					limit 1
                ),
                current_timestamp(),
                null,
                (
					select concat('Accendi ', C.Nome,' ', S.Nome, ' cod: ', C.Codice)
					from programmi P inner join Dispositivi C on P.Dispositivo = C.Codice inner join Stanza S on C.Stanza = S.CodiceStanza
					where consumoMedio <= energiaprodotta * 1000 - energiaconsumata
                    limit 1
				)
			);
    end if;
end $$;
delimiter ;

-- 6
drop event if exists Allarme;
delimiter $$
create event Allarme
on schedule every 5 minute
do
begin
		if exists (
					select 1
					from Serramenti
					where Stato = 0
					) then
					select 'INTRUSIONE';
		end if;
end $$
delimiter ;

drop procedure if exists Allarme;
delimiter $$
create procedure Allarme()
begin
	if exists (select 1 from INFORMATION_SCHEMA.events where STATUS = 'ENABLED' and EVENT_NAME = "Allarme") then
		alter event Allarme disable;
        update serramenti
        set Stato = 0;
	else
		
		update serramenti
        set Stato = 1
        where Stato = 0;
		
        alter event Allarme enable;
		
    end if;
end $$
delimiter ;

call Allarme();



-- 7
#agginta consumo medio
drop trigger if exists AggiuntaConsumoMedio;
delimiter $$
create trigger AggiuntaConsumoMedio
before insert on programmi
for each row
begin
	declare consumoMedioVal int default 0;
    declare potenzaMax varchar(10) default 0;
    declare indice int default 0;
    if exists(select 1 from elementocondizionamento where Codice = new.Dispositivo) then
		#condizionamento
        select Potenza into potenzaMax
		from elementocondizionamento
		where Codice = new.Dispositivo;
		select locate("-", potenzaMax) into indice;
		select substring(potenzaMax, indice + 1) into potenzaMax;
		set new.consumoMedio = (select ConsumoMassimo from elementocondizionamento where Codice = new.dispositivo) * new.Potenza / potenzaMax;
    elseif exists (select 1 from elementoilluminazione where Codice = new.Dispositivo) then
		#luci
		select Intensita into potenzaMax
		from elementoilluminazione
		where Codice = new.Dispositivo;
		select locate("-", potenzaMax) into indice;
		select substring(potenzaMax, indice + 1) into potenzaMax;
		set new.consumoMedio = (select ConsumoMassimo from elementoilluminazione where Codice = new.dispositivo) * new.Potenza / potenzaMax;
	else
		set new.consumoMedio = RAND()*(2000-30+1)+30;
    end if;
end $$
delimiter ;


-- 8
drop procedure if exists NettoMensile;
delimiter $$
create procedure NettoMensile()
begin
	declare sumEnergiaEolica int default 0;
    declare sumEnergiaSolare int default 0;
    declare sumConsumo int default 0;
	select sum(energiaEolica) into sumEnergiaEolica
	from archivioenergia
	where Data > current_date - interval 1 month;
    
	select sum(energiaSolare) into sumEnergiaSolare
	from archivioenergia
	where Data > current_date - interval 1 month;
    
	select sum(energiaConsumata) into sumConsumo
	from archivioenergia
	where Data > current_date - interval 1 month;
    
    select sumEnergiaSolare + sumEnergiaEolica - sumConsumo;
end $$
delimiter ;