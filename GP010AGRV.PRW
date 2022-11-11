//Bibliotecas
#Include "Protheus.ch"
#Include "TopConn.ch"
 
/*/{Protheus.doc} GP010AGRV
Ponto de Entrada após a gravacao dos registros 
@author Jeane Queiroz (Oficina5)
@since 13/01/2021
/*/
 
User Function GP010AGRV
Private _nTipo:=0
Private _cFuncAtu:=0
Private _cFuncNew:=0
Private _cCargAtu:=0
Private _cCargNew:=0

// 1 - alteração de função
// 2 - alteração de cargo
// 3 - ambos

if 		_cFuncAtu <> _cFuncNew .and. _cCargAtu == _cCargNew
		_nTipo:=1
elseif 	_cFuncAtu == _cFuncNew .and. _cCargAtu <> _cCargNew
		_nTipo:=2
elseif 	_cFuncAtu <> _cFuncNew .and. _cCargAtu <> _cCargNew
		_nTipo:=3
Endif

if _nTipo > 0
	FWMsgRun( , {|| lok := RunCont() } ,, 'Enviando e-mail sobre alteração de Função / Cargo.' )
Endif

Return

//------------------------------------------------------------------------------------------------------------------------------------------

Static Function RunCont

Local cTo		:=" "
Local cSubject  :=" "
Local cCC       :=" "  
Local cCO       :=" "
Local cSource   :=" "
Local cAttach	:=" "
Local cBody 	:=" "

// Workflow que envia email com informações da alteração

// define destinatário
cTo:="qualidade@unimeduberaba.com.br;gestaodepessoas@unimeduberaba.com.br"    //"henrique.chelse@unimeduberaba.com.br"  //

// define informações do e-mail
if 		_nTipo==1
		cSubject:= "Alteração de funcionário - Função "
		cBody 	:= "Foi alterada a função do funcionário:<br>"
		cBody   += "Filial:          "+SRA->RA_FILIAL+" - "+Posicione("SM0",1,cEmpAnt+SRA->RA_FILIAL,"M0_FILIAL") 	+"<br>"
		cBody 	+= "Funcionário:     "+SRA->RA_MAT+" - "+SRA->RA_NOMECMP+"<br><br>"
		cBody 	+= "Função anterior: "+_cFuncAtu+" - "+Posicione("SRJ",1,xfilial("SRJ")+_cFuncAtu ,"RJ_DESC")	+"<br>" 
		cBody 	+= "Função atual   : "+_cFuncNew+" - "+Posicione("SRJ",1,xfilial("SRJ")+_cFuncNew ,"RJ_DESC")	+"<br><br><br>" 
elseif 	_nTipo==2
		cSubject:= "Alteração de funcionário - Cargo "
		cBody 	:= "Foi alterada o cargo do funcionário:<br>"
		cBody   += "Filial:         "+SRA->RA_FILIAL+" - "+Posicione("SM0",1,cEmpAnt+SRA->RA_FILIAL,"M0_FILIAL") 	+"<br>"
		cBody 	+= "Funcionário:    "+SRA->RA_MAT+" - "+SRA->RA_NOMECMP+"<br><br>"
		cBody 	+= "Cargo anterior: "+_cCargAtu+" - "+Posicione("SQ3",1,xfilial("SQ3")+_cCargAtu ,"Q3_DESCSUM")	+"<br>" 
		cBody 	+= "Cargo atual   : "+_cCargNew+" - "+Posicione("SQ3",1,xfilial("SQ3")+_cCargNew ,"Q3_DESCSUM")	+"<br><br><br>" 
elseif 	_nTipo==3
		cSubject:= "Alteração de funcionário - Função/Cargo"
		cBody 	:= "Foi efetuada a alteração de função e cargo no cadastro do funcionário:<br>"
		cBody   += "Filial:          "+SRA->RA_FILIAL+" - "+Posicione("SM0",1,cEmpAnt+SRA->RA_FILIAL,"M0_FILIAL") 	+"<br>"
		cBody 	+= "Funcionário:     "+SRA->RA_MAT+" - "+SRA->RA_NOMECMP+"<br><br>"
		cBody 	+= "Função anterior: "+_cFuncAtu+" - "+Posicione("SRJ",1,xfilial("SRJ")+_cFuncAtu ,"RJ_DESC")	+"<br>" 
		cBody 	+= "Função atual   : "+_cFuncNew+" - "+Posicione("SRJ",1,xfilial("SRJ")+_cFuncNew ,"RJ_DESC")	+"<br><br>" 
		cBody 	+= "Cargo anterior:  "+_cCargAtu+" - "+Posicione("SQ3",1,xfilial("SQ3")+_cCargAtu ,"Q3_DESCSUM")	+"<br>" 
		cBody 	+= "Cargo atual   :  "+_cCargNew+" - "+Posicione("SQ3",1,xfilial("SQ3")+_cCargNew ,"Q3_DESCSUM")	+"<br><br><br>" 
Endif

cCC       := ''  
cCO       := ''
cSource   := 'GP010AGRV'	
cAttach	  := ''

cBody += "Data alteração: "+dtoc(SR7->R7_DATA)+"<br>"
cBody += "E-mail automático enviado pelo ambiente SIGAGPE/P.E. GP010AGRV<br>"
cBody += "</font>"
oHtml := TWFHtml():New()
oHtml:lUsaJS := .F.
oHtml:LoadFile( "\workflow\htm\mailunimed4.htm" )
oHtml:ValByName("cBody"	, cBody)

cHtml := oHtml:HtmlCode()
cHtml := STRTRAN( cHtml , CHR(10), "" )

lMailOk :=U_FCFG001(cSubject, cHtml, cTo   , cCC, cCO, cAttach, cSource)   			

oHtml:Free()

if ! lMailOk
	MsgINFO('Erro ao enviar e-mail ',"Erro e-mail")
Endif

Return

//------------------------------------------------------------------------------------------------------------------------------------------
