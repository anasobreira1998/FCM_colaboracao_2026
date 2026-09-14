library(tidyverse)

library(pdftools)

# ------ Leitura do PDF
cadastro <- pdf_text("cadastro.pdf")
cat(cadastro)

# -- Nomes:

nome <- str_extract_all(cadastro, "(?i)(?<=nome:\\s)[^,(]+") %>%
  unlist() %>%
  str_trim() # Remove espaços
nome

# -- Apelidos:

# --------- Extrai primeiro as linhas "nome"
linhas_nome <- str_extract_all(
  cadastro,
  "(?im)^nome:.*$"
) %>%
  unlist()
linhas_nome

# -------------Extrai os apelidos

apelidos <- linhas_nome %>%
  str_remove("(?i)^nome:\\s*[^,(]+") %>% # remoção dos íncio "nome"
  str_remove_all("(?i)\\baka\\b") %>% # remoção dos "akas"
  str_remove_all(nome) %>%  # remoção dos nomes 
  str_extract_all("[^(),]+") %>% # extrai tudo que não é "," e "()"
  lapply(str_trim) %>% # 
  lapply(\(x) x[x != ""]) # remoção dos espaços
apelidos

# -- Data de nascimento:

data_nasc <- cadastro %>%
  str_extract_all("\\b\\d{1,2}[-/](?:\\d{1,2}|[A-Za-z]{3})[-/][0-9.]+(?:d\\.[A-Za-z]\\.?)?") %>% 
  unlist() %>%
  str_replace_all(c("-" = "/", "(?i)dec" = "12")) # padronização das datas para separação por "/" e meses em números
data_nasc

# -- Endereços:

endereco <- cadastro %>%
  str_extract_all("(?im)(?<=endereço:\\s).*?(?=\\s*CEP:|$)") %>%
  unlist() %>%
  str_trim()
endereco

# -- CEPs:

cep <- cadastro %>%
  str_extract_all("\\b\\d{2}\\.?\\d{3}-?\\d{3}\\b") %>%
  unlist() %>%
  str_remove_all("\\.")
cep

# -- Telefones:

telefone <- cadastro %>%
  str_extract_all("(?im)^(tel|telefone):\\s*.*$") %>%
  unlist() %>%
  str_remove("(?i)^(tel|telefone):\\s*") %>%
  str_trim() %>%
    str_remove_all("\\D") %>% 
  str_replace(
    "(\\d{2})(\\d{5})(\\d{4})",
    "(\\1) \\2-\\3"
  ) # padronização dos telefones (xx) xxxxx-xxxx
telefone

# -- CPFs:

cpf <- cadastro %>%
  str_extract_all("(?im)^cpf:\\s*.*$") %>%
  unlist() %>%
  str_remove("(?i)^cpf:\\s*") %>%
  str_remove_all("\\D") %>%
  str_replace(
    "(\\d{3})(\\d{3})(\\d{3})(\\d{2})",
    "\\1.\\2.\\3-\\4"
  ) # padronização dos CPFs xxx.xxx.xxx-xx
cpf

# ------------------------------ Tabela com dados extraídos

apelidos_tabela <- sapply(apelidos, paste, collapse = "; ") # agrupa os apelidos de uma mesma pessoa

tabela <- tibble(
  Nome = nome,
  Apelido = apelidos_tabela,
  Data_Nascimento = data_nasc,
  Endereço = enderecos,
  CEP = cep,
  Telefone = telefones,
  CPF = cpf
)
View(tabela)
