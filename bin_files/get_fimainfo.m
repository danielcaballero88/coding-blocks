%% Octave script to get FIMA info
clear all; clc;

from = 20171201
to   = 20180315

Id  = 1115
%Id = 1295 % FIMA AHORRO PESOS CLASE "A"
%    1163 % FIMA AHORRO PLUS CLASE "A"
%    1672 % FIMA MIX I CLASE A
%    1250 % FIMA PREMIUM CLASE "A"
%    1113 % FIMA RENTA EN PESOS "CLASE A"
%    1174 % FIMA CAPITAL PLUS "CLASE A"
%    1669 % FIMA FDO.COMUN DE INV. ABIERTO PYMES A
%    1112 % FIMA ACCIONES
%    1115 % FIMA PB ACCIONES CLASE A
%    1677 % FIMA RENTA DOLARES I CLASE A
%    1724 % FIMA RENTA DOLARES II CLASE A

linux_command = ['wget -O "datosHistoricosFima.xls" "http://fondosfima.com.ar/personas/herramientas/consultadatoshistoricos?altTemplate=comp_excel&tipo_comp=P&fondos=',num2str(Id),'&fecha_desde=',num2str(from),'&fecha_hasta=',num2str(to),'"'];

system('rm datosHistoricosFima*.*');
system(linux_command);
system('libreoffice --convert-to xlsx datosHistoricosFima.xls');clc;
%system('rm datosHistoricosFima.xls');

prices = xlsread('datosHistoricosFima.xlsx');
%dates   = linspace(1,size(prices),size(prices));

acumulate = prices/prices(1) - 1;

plot(acumulate);
