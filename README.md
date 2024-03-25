# UPDATE: A pasta CleanResults foi renomada já para o nome da pasta pedido pela Sophie. E estou reestrututando ela para rodar o report certinho. 
Também inclui um README file nessa pasta que será útil para a Sophie
É essa a pasta que iremos *copiar* e dar *commmit* para o repositório env-urban.
-----------------------------------------------------------------------------------------------------------------------------------------------------


## Pastas dentro de CleanResults
`Scripts`: cinco arquivos .R usados para fazer o replication. </br>
'VersaoFinal_Replication.R' que define e executa as funções para criar os arquivos base das tabelas 3-6. Note que ele demora para rodar e salva os resulados em diversos arquivos .csv.</br>
'Table2.R', ..., 'Table6.R': scripts que produzem as tabelas finais do report. Usam como base os arquivos .csv salvos em `RegressionResults`.</br>
</br>
`ReplicatedResults`: arquivos .csv com os resultos replicados das tabelas 3-6.</br>
</br>
`FinalTables`: versão final das tabelas a serem colocadas no report e apresentação. Arquivos em .html.</br>
</br>
`OriginalData`: os três arquivos .dta com os dados originais.


Baixando essa pasta apenas, basta alterar o setwd() um pouco e rodar os códigos.

