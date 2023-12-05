/*
linear regression per avere circa la produzione (coerente con il consumo per il periodo)
il meteo cambia il dato di un tot
il consumo si fa con la affidabilita
se domani c'e' un dispositivo che oggi non posso fare lo rimando
*/




drop procedure if exists AnalisiEnergetica;
delimiter $$
create procedure AnalisiEnergetica(in meteo varchar(9), in affidabilita int)
begin 
	declare epoch int default 0;
    declare M float default 0;
    declare Q float default 0;
    declare pred float default 0;
    declare EnergiaProdotta float default 0;
    declare EnergiaConsumata  float default 0;
    declare giorno int default 0;
    declare deltaEnergia int default 0;
    
	
    drop table if exists dataset; #Contiene i dati di archivio energia su cui si fa la predizione
    create table dataset(
		X int,
        Y int
    );
    drop table if exists dispComuni; #elenco dispositivi comuni tra domani e dopodomani
    create table dispComuni(
		disp int
    );
    drop table if exists result; #result set della funzione
    create temporary table result(
		disp int
    );
    
    #Energia Prodotta
    
    set epoch = current_date - interval 1 month - interval 6 month; # {1}si prendono i dati da un mese a questa parte | l'interval 6 month serve per portare la current date a 6 mesi fa per avere la tabella con dati significativi
    
    insert into dataset
	select datediff(Data,epoch) as X, sum(EnergiaEolica + EnergiaSolare) as Y #La prima datediff serve per avere l'intervallo in giorni tra la data e l'epoch
	from archivioenergia
	where Data > epoch
	group by Data;
    
	call LinearRegression(M, Q);#linea che approssima l'andamento della produzione energetica nell'ultimo mese
    #select M,Q;
    set pred = M * (select max(X) + 2 from dataset) + Q; #previsione della produzione di domani (il +2 serve per avere la x che rappresenta domani)
    #select pred - pred/40;#cosi perche mi andava
    
    
    #influenza del tempo
    case meteo
    when 'sole' then
		set pred = pred + (pred / 2) * affidabilita / 100;
    when 'variabile' then set pred = pred;
    when 'pioggia' then
		set pred = pred - (pred / 2) * affidabilita / 100;
    end case;
	
    set EnergiaProdotta = ifnull(pred,0);
    
    truncate dataset;
    
    #Energia Consumata
    
    set epoch =  current_date - interval 5 month - interval 6 month;#5 | l'interval 6 month serve per portare la current date a 6 mesi fa per avere la tabella con dati significativi
    
    insert into dataset
    select datediff(Data,epoch) as X, sum(E.EnergiaConsumata) as Y
	from archivioenergia E
	where weekday(Data) = weekday(current_date() + interval 1 day) and Data > epoch #il giorno e' domani
	group by Data;
    
    call LinearRegression(M,Q);
    
    set EnergiaConsumata = ifnull(M * (select max(X) + 6 from dataset) + Q,0);#6 perche si va avanti di una settimana con la previsione rispetto a +2
    #select EnergiaConsumata;
    #select energiaprodotta;
    insert into dispComuni #dispositivi usati nei giorni della settimana corrispondenti a domani e dopodomani negli ultimi 5 mesi
    select Dispositivo
	from (
		select Dispositivo
		from registri
		where weekday(DataInizio) = weekday(current_date() + interval 1 day) and DataInizio > epoch
		group by Dispositivo
		order by count(*) desc
    ) as L natural join(
		select Dispositivo
		from registri
		where weekday(DataInizio) = weekday(current_date() + interval 2 day) and DataInizio > epoch
		group by Dispositivo
		order by count(*) desc
    ) as R;
    
    set DeltaEnergia = abs(EnergiaConsumata - energiaprodotta);
    if EnergiaConsumata > energiaprodotta then #giorno 1 vuol dire che si deve posticipare qualche dispositivo altrimenti anticipo
		set giorno = 1;
    else
		set giorno = 2;
    end if;

    insert into result
	with ausilio as(
		select C.Dispositivo, avg(P.ConsumoMedio) as med, D.disp, count(*) as conteggio
		from (																																	
			select Dispositivo #I dispositivi che si sono usati di piu' nel giorno della settimana corrispondente a domani                       
			from registri
			where weekday(DataInizio) = weekday(current_date() + interval giorno day)
        ) as C inner join programmi P on C.Dispositivo = P.dispositivo left outer join dispComuni D on C.Dispositivo = D.disp #l'outer join si fa per essere sfruttato nella window funciton sopra
        group by dispositivo
		), res as(
				select *, sum(med) over(rows between unbounded preceding and current row) as diff #somma progressiva dei consumi 
				from (
					(
					select *
					from (
						select *,1 as ordine
						from ausilio A
						where disp is not null #i dispositivi comuni tra domani e dopodomani a consumo maggiore
						)as aux
					where conteggio >= (select max(B.conteggio) from ausilio B where disp is not null) / 2 #limito il numero dei record di dispositivi comuni per mantenere risultati significativi
					)
					union all
					(
						select *, 2 as ordine
						from ausilio A
						where disp is null #i dispositivi piu presenti o domani o dpodomani a seconda di giorno a consumo maggiore 
					)
					order by ordine, conteggio desc, med desc#ordinamento effettivo
				) as end
		)
			select dispositivo
			from res where diff < deltaenergia #prendo record finche' la somma progressiva e' minore del delta
		union
        (
			select dispositivo
			from res where diff > deltaenergia and giorno = 1 #prendo un record in modo da superare il delta nel caso in cui debba rimandare perche devo scendere sotto il delta
			limit 1
		);# controlla che l'ordine sia effettivamente sia mantenuta TODO

    
    if giorno = 1 then #abbellimento del risultato
		select 'Rimanda il', disp
        from result;
	else
		select 'Anticipa il', disp
        from result;
	end if;
end $$
delimiter ;

drop procedure if exists LinearRegression;
delimiter $$
create procedure LinearRegression(out M float, out Q float)
begin
	declare dim int default 0;
    declare mediaX float default 0;
    declare mediaY float default 0;
    declare errXY float default 0;
    declare errXX float default 0;

	set dim = (select count(*) from dataset);
    set mediaX = (select avg(X) from dataset);
    set mediaY = (select avg(Y) from dataset);
    set errXY = (select sum(prod) from (select X * Y as prod from dataset) as A) - dim * mediaX * mediaY;
    set errXX = (select sum(prod) from (select X * X as prod from dataset) as A) - dim * mediaX * mediaX;
    set M = errXY / errXX;
    set Q = mediaY - M * mediaX;
end $$
delimiter ;

call AnalisiEnergetica('pioggia',90);