# Função que cria um plot do tipo abaco automaticamente
# Autor: Leonardo Donato Nunes. Criado em: 12nov2016

# Modificado em: 13nov2016 - Incluí a opção de usar a vazão no gráfico
# Modificado em: 30nov2016 - Incluí a opção de plotar a grande_zona

plotar_abaco <- function(dados, esquema_cores, grande_zona = T, diretorio, nome_arquivo, n = 2, ncol = 3,
                         escala_periodo = "week", legenda = T, dados_vazao, plotar_vazao = F){

  # Carrega o arquivo de dados que contém os registros dos peixes
  a <- dados
  tabela_cores <- esquema_cores
  if(plotar_vazao == T){vazao <- dados_vazao}
  # Teste para saber se a grande zona vai ser usada para o gráfico.
  
  if(grande_zona == T){
    a$nome_zona <- a$grande_zona
  }
  
  # Prepara os dados para serem plotados
  a$data_hora <- as.POSIXct(a$data_hora, format = "%Y-%m-%d %H:%M:%S")
  a$peixe_id <- as.factor(a$peixe_id)
  a$nome_zona <- as.character(a$nome_zona)
  a <- a[order(a$peixe_id, a$data_hora), ]
  
  if(plotar_vazao == T){
  vazao$Data <- as.POSIXct(vazao$Data, format = "%m/%d/%Y")
  vazao <- subset(vazao, vazao$Data > min(a$data_hora))
  }
  # Cria os vetores de valores únicos para serem usados na função "for"
  u1 <- unique(a$peixe_id)
  u2 <- unique(a$nome_zona)
  
  # Cria o dispositivo png para a área de plotagem
  png(filename = paste(diretorio, "/", nome_arquivo, ".png", sep = ""), width = 1300, height = 900)
  
  
  # Inicia o plot ----
  
  # Executa os testes de plotar a vazão e a legenda.
  if(legenda == T & plotar_vazao == T){
    par(cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, mar = c(4,6,4,5))
  }
  if(legenda == F & plotar_vazao == T){
    par(cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, mar = c(4,6,2,5))
  }
  if(legenda == T & plotar_vazao == F){
    par(cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, mar = c(4,6,4,3))
  }
  if(legenda == F & plotar_vazao == F){
    par(cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, mar = c(4,6,2,3))
  }
  
  # Abre o dispositivo de plotagem
  plot(a$data_hora, a$peixe_id, axes = F, ylab = "", xlab = "Data da detecção") # Plot com todos os dados. Sem os eixos
  axis.POSIXct(1, at = seq(min(a$data_hora), max(a$data_hora), 
                           by = escala_periodo), format = "%d/%m/%Y") # Configura o eixo de data e hora
  # Configura o eixo dos códigos dos peixes
  axis(2, at = 1:length(u1), labels = u1, las = 2)
  
  # Texto das margens
  mtext("Código do indivíduo", side = 2, line = 4, cex = 2.3)
  mtext(paste("n = ", length(u1), sep = ""), side = 4, line = -1, las = 2, at = length(u1) + n - 1, cex = 2)
  abline(h = 1:length(u1), lty = 8, col = 8)
  for (i in 1:length(u2)) {
    b <- subset(a, a$nome_zona == as.character(u2[i]))
    points(b$data_hora, b$peixe_id, pch = 15, 
           col = as.character(tabela_cores[tabela_cores$locais == u2[i], ]$cores))
  }
  cor_legenda <- data.frame()
  for (i in 1:length(u2)) {
    d <- as.data.frame(tabela_cores[tabela_cores$locais == u2[i],]$cores, stringsAsFactors = FALSE)
    cor_legenda <- rbind(cor_legenda,d)
  }
 cor <- as.character(cor_legenda$`tabela_cores[tabela_cores$locais == u2[i], ]$cores`)
  if(legenda == T){
  legend(x = min(a$data_hora),
         y = length(u1) + n, xpd = T, ncol = ncol, legend = u2, pch = 15, cex = 1.5, 
         col = cor)
  }
 if(plotar_vazao == T){
   par(new = T)
 }
 
 if(plotar_vazao == T){
   plot(vazao$Data, vazao$Vazao, type = "l", col = 4, axes = F, xlab = "", ylab = "")
   axis(4, cex.axis = 1.2)
   mtext("Vazão média diária (m³/s)", side = 4, line = 3, cex = 2.2)
   }
 
  dev.off()
}
