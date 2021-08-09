# Analysis

# This is where the magic takes place...
# or not since you get to see everything ;D
options(scipen=999)

# Packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(stringi)
library(ggplot2)
library(ggthemes)
library(tm)
library(tidytext)
library(corrplot)
library(Hmisc)

# Bind all data by speaker and year to create one big dataset for each case

# US

# Speeches
load("~/GitHub/Poldis/data/US_oral.rda")
uoral <- US_oral %>% select(speaker, date, text)
uoral$speaker <- paste0(uoral$speaker, "_", stringr::str_extract_all(uoral$date, "^[0-9]{4}")) # speaker year
uoral <- aggregate(uoral$text, list(uoral$speaker), paste, collapse = " ")
uoral <- rename(uoral, doc_id = "Group.1", text = "x")
uoral$setting <- "speeches"

# Campaign
load("~/GitHub/Poldis/data/US_campaign.rda")
ucamp <- US_campaign %>% select(speaker, date, text)
ucamp$speaker <- paste0(ucamp$speaker, "_", stringr::str_extract_all(ucamp$date, "^[0-9]{4}")) # speaker year
ucamp <- aggregate(ucamp$text, list(ucamp$speaker), paste, collapse = " ")
ucamp <- rename(ucamp, doc_id = "Group.1", text = "x")
ucamp$setting <- "campaign"

# Debates
load("~/GitHub/Poldis/data/US_debates.rda")
udeb <- US_debates %>% select(Speakers, Date, Text)
udeb$speaker <- paste0(udeb$Speakers, "_", stringr::str_extract_all(udeb$Date, "^[0-9]{4}")) # speaker year
udeb <- aggregate(udeb$Text, list(udeb$speaker), paste, collapse = " ")
udeb <- rename(udeb, doc_id = "Group.1", text = "x")
udeb$setting <- "debates"

# Interview
load("~/GitHub/Poldis/data/US_interviews.rda")
uint <- US_interviews %>% select(speaker, date, text)
uint$speaker <- paste0(uint$speaker, "_", stringr::str_extract_all(uint$date, "^[0-9]{4}")) # speaker year
uint <- aggregate(uint$text, list(uint$speaker), paste, collapse = " ")
uint <- rename(uint, doc_id = "Group.1", text = "x")
uint$setting <- "interviews"

# Bind
US <- rbind (uoral, ucamp, udeb, uint)

# Brazil
# Speeches
load("~/GitHub/Poldis/data/BR_oral.rda")
boral <- BR_oral %>% select(date, presid, text)
boral$Speaker <- paste0(boral$presid, "_", boral$date) # get speaker year
boral <- aggregate(boral$text, list(boral$Speaker), paste, collapse =" ")
boral <- rename(boral, doc_id = "Group.1", text = "x")
boral$setting <- "speeches"

# Campaign
load("~/GitHub/Poldis/data/BR_campaign.rda")
bcamp <- BR_Campaign %>% select(Speaker, Date, Text)
bcamp$Speaker <- paste0(bcamp$Speaker, "_", bcamp$Date) # get speaker year
bcamp <- aggregate(bcamp$Text, list(bcamp$Speaker), paste, collapse = " ")
bcamp <- rename(bcamp, doc_id = "Group.1", text = "x")
bcamp$setting <- "campaign"

# Debates
load("~/GitHub/Poldis/data/BR_debates.rda")
bdeb <- BR_debates %>% select(Speaker, Date, Text)
bdeb$Date <- stringr::str_extract(bdeb$Date, "[0-9]{4}")
bdeb$Speaker <- paste0(bdeb$Speaker, "_", bdeb$Date) # get speaker year
bdeb <- aggregate(bdeb$Text, list(bdeb$Speaker), paste, collapse = " ")
bdeb <- rename(bdeb, doc_id = "Group.1", text = "x")
bdeb$setting <- "debates"

# Interviews
load("~/GitHub/Poldis/data/BR_interviews.rda")
bint <- BR_Interviews %>% select(Speaker, Date, Text)
bint$Speaker <- paste0(bint$Speaker, "_", bint$Date) # get speaker year
bint <- aggregate(bint$Text, list(bint$Speaker), paste, collapse = " ")
bint <- rename(bint, doc_id = "Group.1", text = "x")
bint$setting <- "interviews"

# Bind
BR <- rbind(boral, bcamp, bdeb, bint)

# some simple cleaning (this took a few hours)
# remove all punctuations. Because this takes a while
# Rds versions of this data can be found in the data folder.
# They are called US.Rds and BR.Rds. You can load these if
# you do not want to run the following lines (93-110).
textus <- purrr::map(US$text, as.character)
textus <- gsub("[[:punct:]]", "", textus, perl=TRUE)
textus <- tolower(textus) # lower case
textus <- stringr::str_squish(textus) # remove extra white spaces for all
US$text <- textus
textbr <- purrr::map(BR$text, as.character)
textbr <- gsub("[[:punct:]]", "", textbr, perl=TRUE)
# standardize certain words for Brazil to improve matching
textbr <- gsub("^é$", "e", textbr, ignore.case = TRUE, perl = TRUE)
textbr <- gsub("^à$", "a", textbr, ignore.case = TRUE, perl = TRUE)
textbr <- gsub("nao", "não", textbr, ignore.case = TRUE, perl = TRUE)
textbr <- gsub("sao", "são", textbr, ignore.case = TRUE, perl = TRUE)
textbr <- gsub("^nos$", "nós", textbr, ignore.case = TRUE, perl = TRUE)
textbr <- tolower(textbr) # lower case
textbr <- stringr::str_squish(textbr) # remove extra white spaces for all
BR$text <- textbr

# Let's create a length column based on number of characters for later on
BR$length <- nchar(BR$text)
US$length <- nchar(US$text)
# Let's also create a date (year) column for later on
BR$date <- stringr::str_extract(BR$doc_id, "[0-9]{4}")
US$date <- stringr::str_extract(US$doc_id, "[0-9]{4}")
# let' create a setting variable for when data is merged
US$settingc <- paste0(US$setting, "_US")
BR$settingc <- paste0(BR$setting, "_BR")

# Load dictionaries and get frequencies for speaker-year
# Shall we start with the US?

# Dictionary of terms for authenticity performances for US
truth_telling <- ("am telling the truth|are telling the truth|is telling the truth|the truth is|this is the truth|not lying|not lies|no lies|not telling you lies|is honest|am honest|is being honest|are being honest|are honest|honesty|is sincere|are sincere|am sincere|is being sincere|are being sincere|is true|are true|not a liar|bottom of my heart|I swear|I reassure|we reassure|I assure|we assure|be assured|is truthful|are truthful|am truthful|is being truthful|are being truthful|I know that|is evident|are evident|I am sure|trust me|am frank|are frank|is frank|being frank|is upfront|are upfront|am upfront|being upfront|will come clean|am coming clean|are coming clean|is straightforward|are straightforward|being straightforward|believe me|I am certain|no bullshit|not bullshitting")
truth_telling <- stringr::str_squish(truth_telling) # removes unseen white spaces
lie_accusations <- ("not truth|not the truth|not true|aren’t true|isn’t true|being untruthful|is lying|are lying|is a liar|are liars|is dishonest|are  dishonest|being dishonest|is fake|are fake|being fake|is corrupt|are corrupt|full of lies|not sincere|not being sincere|isn’t sincere|aren’t sincere|not honest|not being honest|is cheating|is a cheater|are cheaters|are cheating|are tricking|is tricking|be deceived|is deceiving|are deceiving|are a hypocrite|is a hypocrite|are being a hypocrite|is being a hypocrite|is crooked|are crooked|is misleading|are misleading|has double-standards|are sneaky|is sneaky| has two faces|two-faced|has double faces|double-faced|you are wrong|not correct|fooled by|do not believe|is misrepresenting|they misrepresent|is misrepresent|are misrepresent|pretends that|pretends to|is pretending|are pretending|keep pretending|breach your trust|breach of trust|is false|are false|being false|is misinforming|are misinforming|being misinformed|pretended|cut the crap|full of crap")
lie_accusations <- stringr::str_squish(lie_accusations)
consistency <- ("we delivered|I delivered|check and see|I keep my word|we keep our word|I kept my word|we kept our word|I keep my promise|I kept my promise|we keep our promise|as promised|we kept our promise|am responsible|I take responsibility|we take responsibility|we assume responsibility|we are accountable|we are responsible|our duty|my duty|give my word|giving my word|own up my|owning up my|accept responsibility|accept the blame|recognize my mistakes|admit I was wrong|I made mistakes|I guarantee|we guarantee|I can guarantee|we can guarantee|I promise|we promise|we can prove|I can prove|we proved|I proved|am reliable|rely on me|rely on us|be reassured|you can hold me accountable|you can hold us accountable|see with your own eyes|vote of confidence|our mission|my mission|my commitment|our commitment|during our government|during my government|while I was in charge")
consistency <- stringr::str_squish(consistency)
finger_pointing <- ("are inconsistent|is inconsistent|being inconsistent|are irresponsible|is irresponsible|being irresponsible|their fault|not my fault|not our fault|they left us with|they are responsible|are not responsible|aren’t responsible|is not responsible|isn’t responsible|costed us|false promises|lack accountability|lacking accountability|not kept their word|not kept his word|not kept her word|not kept promises|not kept the|not kept his|not kept her|not kept their|not keep their word|not keep his word|not keep her word|not keep the|didn’t keep the|didn’t keep her|didn’t keep his|hasn’t kept his|hasn’t kept her|not recognize|he made mistakes|she made mistakes|they made mistakes|not our mistake|not my mistake|not take responsibility|not my responsibility|not accountable|him accountable|them accountable|her accountable|blame them|blame him|blame his|blame her|their blame|break promises|broken promises|has betrayed|they betrayed|betraying|will betray|has tricked|has lied|not deliver|didn’t deliver|hasn’t deliver|failed your obligations|failed in your obligations|failed his obligations|failed her obligations|failed in his dut|failed in her dut|failed his dut|failed her dut|failed your dut|stabbed in the back")
finger_pointing <- stringr::str_squish(finger_pointing)
origins <- ("I was born|I come from|we come from|I grew up|growing up in|my parents|my mom|my mother|my father|my dad|my family|raised me|I was raised|we were raised|we grew up|my background|being surrounded by|being exposed to|my siblings|going to school in|our local church|Sunday mass|Saturday mass|family tradition|tradition in my house|in our house|growing up|back in the day|my grandparents|in my town|in my state|in my region|our community|in my community|our town|our state|my hometown|our hometown|my home state|our home state|back home|our house|my house|our neighbourhood|in my district|I lived in|we lived in|we used to play|I used to play|I was thought")
origins <- stringr::str_squish(origins)
common_sense <- ("is common sense|are common sense|everyone knows|it is undeniable|stating the obvious|say the obvious|everyone agrees|we all know|common wisdom|the people know|popular knowledge|from experience|it is my experience|sound judgment|practical solution|practical choice|practical answer|pragmatic solution|pragmatic answer|pragmatic choice|realistic answer|let me tell you about|is obvious|are obvious|obvious answer|obvious solution|as we all learned|we have all learned that|do not need to tell you that|the reality is|there is no logic|it does not make sense|it doesn’t make sense|we know it does not work|no one disagrees that|no person disagrees|there is not a person|there is not a human being|there is not a family|there is not an American|there is no single citizen|there is not one single person|there is not one single human being|there is not one single family|there is not one single American|there is not one single citizen|there is not one single person|there is not one human being|there is not one family|there is not one American")
common_sense <- stringr::str_squish(common_sense)
anti_pc<- ("politically correct|political correctness|PC|plain speaking|speaking my mind|speak my mind|say what I think|saying what I think|not going to pretend|not pretend|speak what you think|not what you want to hear|not butter up|not beat around the bush|cut to the chase|just being real|saying what everyone thinks|say what everyone is thinking|speaking plainly|coloured people|negro|retarded|nigger|third world|oriental people|crippled people|is crippled|culturally deprived|drug addict|junkie|drunk|fat people|fat person|fat population|handicapped|homosexual|faggot|deviant|perverted|illegals|illegal immigrants|illegal alien|^jew$|^jews$|non-white|prostitutes|promiscuous|stupid|tribe|underdeveloped")
anti_pc <- stringr::str_squish(anti_pc)
territory <- ("have been to|have visited|came all the way to|back from|will visit|saw first-hand|see first-hand|we visited|I visited|we visited|travelled to|traveling to|spend a few days in|spent some time in|spent time in|met great people in|we were hosted|I was hosted|our time in|my time in|our visit|spent a lot of time in|were many times in|got to know the whole country|got to know all the states")
territory <- stringr::str_squish(territory)

# Get each performance as frequencies per row, the (?i) makes it case insensitive
US$truth <- stringr::str_count(US$text, paste0("(?i)", truth_telling))
US$lies <- stringr::str_count(US$text, paste0("(?i)", lie_accusations))
US$consistency <- stringr::str_count(US$text, paste0("(?i)", consistency))
US$fpoint <- stringr::str_count(US$text, paste0("(?i)", finger_pointing))
US$origins <- stringr::str_count(US$text, paste0("(?i)", origins))
US$common_sense <- stringr::str_count(US$text, paste0("(?i)", common_sense))
US$anti_PC <- stringr::str_count(US$text, paste0("(?i)", anti_pc))
US$territory <- stringr::str_count(US$text, paste0("(?i)", territory))

# take a quick look at the findings
US_1995 <- US %>%  filter(date == 1995) %>% select(-text)
US_1995
US_2015 <- US %>%  filter(date == 2015) %>% select(-text)
US_2015

# Get obs per year and plot
truth_time <- US %>%
  select(truth, date, setting, length) %>%
  mutate(n_truth = truth/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_truth))
ggplot(truth_time, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# interesting spike later years

lies_time <- US %>%
  select(lies, date, setting, length) %>%
  mutate(n_lies = lies/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_lies))
ggplot(lies_time, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# also spike in later years

antipc_time <- US %>%
  select(anti_PC, date, setting, length) %>%
  mutate(n_PC = anti_PC/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_PC))
ggplot(antipc_time, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# interesting as well

# table the findings by year and setting
aut_perf_time <- US %>%
  select(-c(doc_id, setting, settingc, text)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(length, truth, lies, fpoint,anti_PC))
aut_perf_time_long <- aut_perf_time %>%
  tidyr::pivot_longer(consistency:anti_pc, "Performance")
aut_perf_time_long$date2 <- stringr::str_extract(aut_perf_time_long$date, "[0-9]{2}$")
aut_perf_time_long$value2 <- aut_perf_time_long$value*100000
US_aut_perf <- ggplot(aut_perf_time_long, aes(x = reorder(date2, as.numeric(date)), y = value2 , fill = Performance)) +
  geom_line(aes(group = Performance)) +
  geom_point(size = 4, shape = 21) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances in the US in Time",
       subtitle = "Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight()
US_aut_perf
# Cool stuff!!!

aut_perf_setting <- US %>%
  select(-c(doc_id, text, settingc)) %>%
  group_by(date, setting) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(length, truth, lies, fpoint,anti_PC))
aut_perf_set_long <- aut_perf_setting %>%
  tidyr::pivot_longer(consistency:anti_pc) %>%
  group_by(setting, date) %>%
  summarise(value = sum(value))
aut_perf_set_long$date2 <- stringr::str_extract(aut_perf_set_long$date, "[0-9]{2}$")
ggplot(aut_perf_set_long, aes(x = reorder(date2, as.numeric(date)), y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 3, shape = 21) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in the US in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()
US_setting_time <- ggplot(aut_perf_set_long, aes(x = reorder(date2, as.numeric(date)), y = value, fill = Setting)) +
  geom_line(aes(group = Setting, color = Setting), size = 1.2) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in the US in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()
US_setting_time
# Not at all as expected... but interesting!!!

# Brazil

# Same dictionary but in Portuguese
# I am keeping the dictionary separate and passing each dataset to disctionary in respective
# language to avoid overlap and not risk some misclassifications.
truth_telling <- ("a verdade e|esta e a verdade|digo a verdade|dizemos a verdade|pura verdade|não e mentira|não estou mentindo|e honesto|sou honesto|somos honesto|sendo honesto|a honestidade|ser sincero|e sincero|com sinceridade|e verdade|são verdadeiras|não sou mentiroso|não minto|fundo do meu coração|sou verdadeiro|somos verdadeiros|tenho certeza|certeza absoluta|confia em mim|confie em mim|pode confiar|sou franco|somos francos|fraqueza|falando a verdade|falo a verdade|falamos a verdade|acredite em mim|pode acreditar|podem acreditar|eu tenho certeza|isso e a verdade|somos honestos|com honestidade|toda a sinceridade|com sinceridade|toda sinceridade|sou confiável|somos confiáveis|as coisas são assim|a realidade das coisas|juro por deus|com certeza|digo com precisão|veracidade|premissa|afirmo para vocês|isso e como aconteceu|falar umas verdades")
truth_telling <- stringr::str_squish(truth_telling) # removes unseen white spaces
lie_accusations <- ("não e verdade|não e verdadeiro|e mentiroso|está mentindo|são mentiroso|e mentira|de mentira|tudo mentira|e desonesto|mentiram|mentiu|um desonesto|esse desonesto|de desonesto|são desonesto|e falso|são falsos|são corruptos|e corrupto|de corrupto|todos corrupto|não são sincero|não e sincero|não são honestos|não e honesto|são trapaceiros|e trapaceiro|eles trapaceiam|trapaceou|e enganar|ser enganado|vão enganar|sendo enganados|e hipócrita|e enganador|e enganação|duas caras|enganado por|não acredite|eles finge|ele finge|e fingimento|ela finge|quebrou a sua confiança |quebra de confiança| e falso|são falsos|falsidade|e ficção|história para boi dormir|historinha para boi dormir|e calunia|são calunias|difamação|difamar|uma inverdade|são inverdades|e inverdade|isso e invenção|essas são invenções|isso e uma lenda|essas são ledas|tenta iludir|tentando iludir|uma farsa|tramoia|mal intencionado|mas intenções|falta de informação|esta mal-informado|estão mal-informados")
lie_accusations <- stringr::str_squish(lie_accusations)
consistency <- ("nós entregamos|eu entreguei|veja com seus próprios olhos|cumpro minhas palavras|cumprimos nossas palavra|cumpri minha palavra|cumpro minhas promessas|nossas promessa|um compromisso|meu compromisso|tenho um compromisso com|eu sou responsável|eu assumo a responsabilidade|nós somos responsáveis|nós assumimos a responsabilidade|nosso dever|meu dever|dou minha palavra|faço uma promessa|fazer uma promessa|aceitar a responsabilidade|aceito a responsabilidade|aceitamos a responsabilidade|aceitar a culpa|meus erros|que errei|eu errei|eu garanto|eu posso garantir|eu prometo|podemos provar|posso provar|provaremos|eu provei|voto de confiança|encarrego pessoalmente|encarreguei pessoalmente|estou comprometido|meu comprometimento|comprometimento com|o comprometimento|fazer o possível|minha supervisão|minha missão|nossa missão|no meu governo|no nosso governo|durante nosso governo|eu era encarregado|eu era o encarregado|fomos encarregados de")
consistency <- stringr::str_squish(consistency)
finger_pointing <- ("e inconsistente|são inconsistente|e irresponsável|são irresponsáveis|culpa deles|a culpa não e minha|não e minha culpa|eles nós deixaram|são responsáveis|e responsável|nós custou|falsas promessas|falta de prestação de contas|falharam|falhou|não cumpriu|não cumpriram|não reconheceu|não reconheceram|errou|erraram|não se responsabiliza|não me responsabilizo|culpa e sua|sua culpa|quebrar promessas|promessas quebradas|quebra de promessas|fala uma coisa e faz outra|fala uma coisa aqui e faz outra|falsas promessas|são trapaceiros|cometeu erros|cometeram erros|não reconhece|não reconheceu|assumiu a responsabilidade|promete uma coisa|promete o mundo|traiu a confiança|traiu a sua confiança|quebra de confiança|quebraram sua confiança|e falcatrua|foi falcatrua|cheio de falcatrua|houve fraude|houveram fraudes|fraudulento|uma negociata|facada nas costas|faltou com respeito|não faz o que promete|não fez o que promete|promessas em vão|palavras em vão|falta de comprometimento|falta de compromisso|houveram desvio|houve desvio|a culpa e do|cheio de promessas|a conta não fecha|não terminaram")
finger_pointing <- stringr::str_squish(finger_pointing)
origins <- ("Eu nasci|Eu vim de|eu venho de|viemos de|cresci|nós crescemos|meus pais|minha mãe|minha mãe|minha família|fui criado|fomos criados|minhas origens|meus irmãos|meu irmão|minha irmã|tradição familiar|tradição em casa|crescendo|antigamente|meu avô|minha avó|meus avós|na minha cidade|no meu estado|na minha região|nossa comunidade|na minha comunidade|nossa cidade|nosso estado|cidade natal|estado de origem|minha casa|nossa casa|lá em casa|nosso bairro|no meu bairro|eu morava|vivíamos|na minha terra|de onde eu venho|missa de domingo|missa toda semana|brincava|eram outros tempos|fui educado|morávamos|eu morei|nós moramos|de onde venho|eram tempos diferentes")
origins <- stringr::str_squish(origins)
common_sense <- ("senso comum|bom senso|todos sabem|afirmando o óbvio|todos concordam|todos sabemos|sabemos todos|todos nós sabemos|sabedoria popular|por experiência|e minha experiência|sou prático|tem que ser prático|devemos ser prático|sendo prático|sou pragmático|tem que ser pragmático|devemos ser  pragmático|sendo pragmático|sou realista|sendo realista|sejamos realista|realisticamente falando|e óbvio|como todos nós aprendemos|como sabemos|não preciso te dizer|o povo sabe|agente aprendeu|nós aprendemos|nós sabemos|não tem logica|como aprendemos|não faz sentido|não fazem sentido|estamos cansados de saber|sabemos que não funciona|ninguém discorda que|não tem uma pessoa|não existe uma pessoa|não há uma pessoa|não existe um ser humano|não tem um ser humano|não há um ser humano|não tem uma família|não existe uma família|não há uma família|não tem um brasileiro|não há um Brasileiro|não existe um brasileiro|não tem uma brasileira|não há uma Brasileira|não existe uma brasileira")
common_sense <- stringr::str_squish(common_sense)
anti_pc<- ("politicamente correto|falar francamente|falando francamente|falar o que penso|falo o que penso|falando o que penso|dizer o que penso|papas na língua|não vou fingir|não estou aqui para agradar|falar o que você pensa|o que você quer ouvir|não adulterar|não rodeio|não dou rodeio|direto ao ponto|dizer o que todos pensam|dizendo o que penso|dizendo o que todos pensam|dizer o que todos estão pensando|não vou amaciar|não dá para amaciar|gordos|retardado|retardada|veado|população preta|os pretos|as pretas|terceiro mundo|viciado em drogas|bêbado|drogado|sem cultura|pervertidos|promíscuo|imbecil|estupido|aleijado|defeituoso|incapacitado|inválido|mongoloide|deficiente mental|deficiência mental|o incapacitado|a incapacitada|travesti|homossexualismo")
anti_pc <- stringr::str_squish(anti_pc)
territory <- ("estive em|visitei|voltou de|voltei de|voltando de|voltamos de|estive em|estivemos em|visitará|visitarei|vi em primeira mão|ver em primeira mão|visitamos|viajei para|passei alguns dias em|passei algum tempo em|passei um tempo|conheci ótimas pessoas|conhecemos ótimas pessoas em|fomos hospedados|minha passagem|nossa passagem|nossa visita|fui muitas vezes para|estive muitas vezes em|passei muito tempo em|meu tempo em|estive por todo o Brasil|de norte a sul do pais|conheço todo o pais|conheci todo o pais|conheci todo o Brasil|conheço todo o Brasil")
territory <- stringr::str_squish(territory)

# Get each performance as frequencies per row, the (?i) makes it case insensitive
BR$truth <- stringr::str_count(BR$text, paste0("(?i)", truth_telling))
BR$lies <- stringr::str_count(BR$text, paste0("(?i)", lie_accusations))
BR$consistency <- stringr::str_count(BR$text, paste0("(?i)", consistency))
BR$fpoint <- stringr::str_count(BR$text, paste0("(?i)", finger_pointing))
BR$origins <- stringr::str_count(BR$text, paste0("(?i)", origins))
BR$common_sense <- stringr::str_count(BR$text, paste0("(?i)", common_sense))
BR$anti_PC <- stringr::str_count(BR$text, paste0("(?i)", anti_pc))
BR$territory <- stringr::str_count(BR$text, paste0("(?i)", territory))

# take a quick look at the findings
BR_1995 <- BR %>%  filter(date == 1995) %>% select(-text)
BR_1995
BR_2015 <- BR %>%  filter(date == 2015) %>% select(-text)
BR_2015

# Get obs per year and plot
consistency_time <- BR %>%
  select(consistency, date, setting, length) %>%
  mutate(n_consistency = consistency/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_consistency))
ggplot(consistency_time, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# interesting spike later years

fpoint_time <- BR %>%
  select(fpoint, date, setting, length) %>%
  mutate(n_fpoint = fpoint/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_fpoint))
ggplot(fpoint_time, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# also spike in later years

antipc_time_BR <- BR %>%
  select(anti_PC, date, setting, length) %>%
  mutate(n_PC = anti_PC/length) %>%
  group_by(setting, date) %>%
  summarize(value = sum(n_PC))
ggplot(antipc_time_BR, aes(x = date, y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21)
# interesting as well

# table the findings by year and setting
aut_perf_time_BR <- BR %>%
  select(-c(doc_id, setting, text, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(length, truth, lies, fpoint,anti_PC))
aut_perf_time_long_BR <- aut_perf_time_BR %>%
  tidyr::pivot_longer(consistency:anti_pc, "Performance")
aut_perf_time_long_BR$date2 <- stringr::str_extract(aut_perf_time_long_BR$date, "[0-9]{2}$")
aut_perf_time_long_BR$value2 <- aut_perf_time_long_BR$value*100000 #Facilitate visualization
BR_aut_perf <- ggplot(aut_perf_time_long_BR, aes(x = reorder(date2, as.numeric(date)), y = value2 , fill = Performance)) +
  geom_line(aes(group = Performance)) +
  geom_point(size = 4, shape = 21) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances in the Brazil in Time",
       subtitle = "Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight()
BR_aut_perf

aut_perf_setting_BR <- BR %>%
  select(-c(doc_id, text, settingc)) %>%
  group_by(date, setting) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(length, truth, lies, fpoint,anti_PC))
aut_perf_set_long_BR <- aut_perf_setting_BR %>%
  tidyr::pivot_longer(consistency:anti_pc) %>%
  group_by(setting, date) %>%
  summarise(value = sum(value))
aut_perf_set_long_BR$date2 <- stringr::str_extract(aut_perf_set_long_BR$date, "[0-9]{2}$")
ggplot(aut_perf_set_long_BR, aes(x = reorder(date2, as.numeric(date)), y = value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 3, shape = 21) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in the Brazil in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()
BR_setting_time <- ggplot(aut_perf_set_long_BR, aes(x = reorder(date2, as.numeric(date)), y = value, fill = Setting)) +
  geom_line(aes(group = Setting, color = Setting), size = 1.2) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in Brazil in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()
BR_setting_time

# Let's compare Brazil and the US for authenticity performances across settings
aut_perf_set_long$Setting <- paste0(aut_perf_set_long$setting, "_US")
aut_perf_set_long_BR$Setting <- paste0(aut_perf_set_long_BR$setting, "_BR")
all_setting_ap <- rbind(aut_perf_set_long, aut_perf_set_long_BR)
all_setting_ap$value <- all_setting_ap$value*100000 # to facilitate visualization
ggplot(all_setting_ap, aes(x = reorder(date2, as.numeric(date)), y = value , fill = Setting)) +
  geom_line(aes(group = Setting)) +
  geom_point(size = 4, shape = 21) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in Brazil and the US in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()
ggplot(all_setting_ap, aes(x = reorder(date2, as.numeric(date)), y = value, fill = Setting)) +
  geom_line(aes(group = Setting, color = Setting), size = 1.2) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances by Setting in Brazil and the US in Time",
       subtitle = "Normalized by number of characters per year and setting in dataset") +
  theme_fivethirtyeight()

#Let's also add a few of these plotes together for comparison
gridExtra::grid.arrange(US_aut_perf, BR_aut_perf)
# ggpubr::ggarrange(US_aut_perf, BR_aut_perf, nrow = 2, common.legend = TRUE, legend="bottom")
# Interesting!!!
gridExtra::grid.arrange(US_setting_time, BR_setting_time)

# How about we create a stacked bar plot for both cases in time.
US_time <- US %>%
  select(-c(doc_id, text, setting, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(truth, lies, fpoint,anti_PC))

US_time_l <- US_time %>%
  select(-length) %>%
  tidyr::pivot_longer(consistency:anti_pc) %>%
  group_by(date)

US_time_l$date2 <- stringr::str_extract(US_time_l$date, "[0-9]{2}$")
US_time_l <- rename(US_time_l, Performance = name)
US_time_l$value2 <- US_time_l$value*100

US_ttt <- ggplot(US_time_l, aes(reorder(date2, -c(as.numeric(date))), value2, fill = Performance)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Authenticity Performance in the US",
       subtitle = "Normalized by number of characters per year") +
  theme_fivethirtyeight() +
  coord_flip()
US_ttt
# more random than expected

# Let's do the same for Brazil
BR_time <- BR %>%
  select(-c(doc_id, text, setting, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000) %>%
  select(-c(truth, lies, fpoint,anti_PC))

BR_time_l <- BR_time %>%
  select(-length) %>%
  tidyr::pivot_longer(consistency:anti_pc) %>%
  group_by(date)

BR_time_l$date2 <- stringr::str_extract(BR_time_l$date, "[0-9]{2}$")
BR_time_l <- rename(BR_time_l, Performance = name)
BR_time_l$value2 <- BR_time_l$value*100

BR_ttt <- ggplot(BR_time_l, aes(reorder(date2, -c(as.numeric(date))), value2, fill = Performance)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances in Brazil",
       subtitle = "Normalized by number of characters per year") +
  theme_fivethirtyeight() +
  coord_flip()
BR_ttt

# See both plots at the same time
ggpubr::ggarrange(US_ttt, BR_ttt, ncol=2, common.legend = TRUE, legend="bottom")

# How about we compare authenticity and sentiment visualy?
# Sentiment Brazil
sent_BR <- BR %>% select(doc_id, setting, text)
sent_BR$doc_id <- paste0(sent_BR$doc_id, "_", sent_BR$setting)
sent_BR <- VCorpus(DataframeSource(sent_BR))
sent_BR <- tidy(sent_BR)
sent_BR <- sent_BR %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Get Afinn
sent_BR_af <- inner_join(sent_BR, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = sum(n)) %>%
  arrange(id)
sent_BR_af$date <- stringr::str_extract(sent_BR_af$id, "[0-9]{4}")
sent_BR_af <- sent_BR_af %>%
  select(-id) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  arrange(date) %>%
  mutate(case = "BR_sentiment")
sent_BR_af$length <- BR_time$length
sent_BR_af <- sent_BR_af %>%
  mutate(value = (sentiment/length)*40) %>% # to facilitate visualization
  select(-c(sentiment, length))

# Sentiment US
sent_US <- US %>% select(doc_id, setting, text)
sent_US$doc_id <- paste0(sent_US$doc_id, "_", sent_US$setting)
sent_US <- VCorpus(DataframeSource(sent_US))
sent_US <- tidy(sent_US)
sent_US <- sent_US %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Get Afinn
sent_US_af <- inner_join(sent_US, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = sum(n)) %>%
  arrange(id)
sent_US_af$date <- stringr::str_extract(sent_US_af$id, "[0-9]{4}")
sent_US_af <- sent_US_af %>%
  select(-id) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  arrange(date) %>%
  mutate(case = "US_sentiment")
sent_US_af$length <- US_time$length
sent_US_af <- sent_US_af %>%
  mutate(value = (sentiment/length)*40) %>% # to facilitate visualization
  select(-c(sentiment, length))

# Wrangle datesets before binding it
BR_time_ap <- BR_time %>%
  mutate(value = consistency + origins + common_sense +
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc,
         case = "BR_authenticity") %>%
  select(-c(consistency, origins, common_sense,
            territory, truth_telling, lie_accusations,
            finger_pointing, anti_pc, length))
# BR_time_ap$value <- BR_time_ap$value*100 # facilitate visualization

US_time_ap <- US_time %>%
  mutate(value = consistency + origins + common_sense +
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc,
         case = "US_authenticity") %>%
  select(-c(consistency, origins, common_sense,
            territory, truth_telling, lie_accusations,
            finger_pointing, anti_pc, length))
# US_time_ap$value <- US_time_ap$value*100 # facilitate visualization

# Bind all
sent_all <- rbind(sent_BR_af, sent_US_af, BR_time_ap, US_time_ap)
sent_all$date2 <- stringr::str_extract(sent_all$date, "[0-9]{2}$")

# Plot
ggplot(sent_all, aes(x = reorder(date2, as.numeric(date)), y = value, fill = case)) +
  geom_line(aes(group = case, color = case), size = 1.2) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances versus Sentiment in Brazil and the US in Time",
       subtitle = "Normalized by number of characters per year in dataset",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()

# What if we plot simply authnticity in time as a line plot?
# Nothing else, just this, a simple line plot for once
US_time_s <- US_time_ap
US_time_s$case <- "US"
BR_time_s <- BR_time_ap
BR_time_s$case <- "BR"
all_simple <- rbind(US_time_s, BR_time_s)
all_simple$date2 <- stringr::str_extract(all_simple$date, "[0-9]{2}$")
# Plot
ggplot(all_simple, aes(x = reorder(date2, as.numeric(date)), y = value, fill = case)) +
  geom_line(aes(group = case, color = case), size = 1.2) +
  labs(x = "",
       y = "",
       title = "Authenticity Performances in Brazil and the US in Time",
       subtitle = "Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight()

# Lastly, let's see who performs authenticity more, when and how...
# One performce at a time
# There is a little bug with data, fix when you have time!
BR_bug <- BR_ap %>% select(-text) %>% filter(setting != "debates")
bb <- BR_ap  %>% select(-text) %>% filter(setting == "debates") %>%
  select(-c(date, setting, settingc))
bb$doc_id <- stringr::str_remove(bb$doc_id, "-[0-9]{2}-[0-9]{2}")
bb <- bb %>%
  group_by(doc_id) %>%
  summarise(across(everything(), sum))
bb$setting <- "debates"
bb$settingc <- "debates_BR"
bb$date <- str_extract(bb$doc_id, "[0-9]{4}")
BR_pp <- rbind(BR_bug, bb)

# Remove text
# BR_pp <- select(BR_ap, -text)
US_pp <- select(US_ap, -text)

# Merge datasets
ap_all <- rbind(BR_pp, US_pp)

# Normalize
ap_all_n <- ap_all %>%
  mutate(truth_telling = (truth/length)*1000,
       lie_accusations = (lies/length)*1000,
       consistency = (consistency/length)*1000,
       finger_pointing = (fpoint/length)*1000,
       origins = (origins/length)*1000,
       common_sense = (common_sense/length)*1000,
       anti_pc = (anti_PC/length)*1000,
       territory = (territory/length)*1000,
       speaker = stringr::str_remove(doc_id, "_[0-9]{4}$")) %>%
  select(-c(truth, lies, fpoint, anti_PC, length, doc_id))
ap_all_n$speaker <- gsub("Albert Gore, Jr.", "Gore", ap_all_n$speaker)
ap_all_n$speaker <- gsub("Hillary Clinton", "H_Clinton", ap_all_n$speaker)
ap_all_n$speaker <- gsub("George W. Bush", "W_Bush", ap_all_n$speaker)
ap_all_n$speaker <- stringr::word(ap_all_n$speaker, -1) # standardises some speaker names

# Let's get the top 8 performers for each Authenticity Performance

# before we begin, let's try and see a few frequencies for certain speakers
trump <- ap_all_n %>%
  filter(speaker == "Trump", date == 2016) %>%
  select(-c(setting, speaker, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(speaker = "Trump_2016",
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-date) %>%
  relocate(speaker, truth_telling, lie_accusations, consistency, finger_pointing)
clinton <- ap_all_n %>%
  filter(speaker == "H_Clinton", date == 2016) %>%
  select(-c(setting, speaker, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(speaker = "H_Clinton_2016",
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-date) %>%
  relocate(speaker, truth_telling, lie_accusations, consistency, finger_pointing)
clinton_trump_2016 <- rbind(trump, clinton)
# Super interesting!
# Is there a relationship between authenticity performances and being elected?
bolsonaro <- ap_all_n %>%
  filter(speaker == "Bolsonaro", date == 2018) %>%
  select(-c(setting, speaker, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(speaker = "Bolsonaro_2018",
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-date) %>%
  relocate(speaker, truth_telling, lie_accusations, consistency, finger_pointing)
haddad <- ap_all_n %>%
  filter(speaker == "Haddad", date == 2018) %>%
  select(-c(setting, speaker, settingc)) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  mutate(speaker = "Haddad_2018",
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-date) %>%
  relocate(speaker, truth_telling, lie_accusations, consistency, finger_pointing)
bolsonaro_haddad_18 <- rbind(bolsonaro, haddad)
# cool!

# Let's get the top 8 authenticity performances for each performance for both cases
truth_8 <- ap_all_n %>%
  arrange(-truth_telling) %>%
  slice_head(n = 8) %>%
  select(truth_telling, date, speaker, setting) %>%
  mutate(value = truth_telling,
         Performance = "truth_telling") %>%
  select(-truth_telling)
lie_8 <- ap_all_n %>%
  arrange(-lie_accusations) %>%
  slice_head(n = 8) %>%
  select(lie_accusations, date, speaker, setting) %>%
  mutate(value = lie_accusations,
         Performance = "lie_accusations") %>%
  select(-lie_accusations)
consistency_8 <- ap_all_n %>%
  arrange(-consistency) %>%
  slice_head(n = 8) %>%
  select(consistency, date, speaker, setting) %>%
  mutate(value = consistency,
         Performance = "consistency") %>%
  select(-consistency)
fpoint_8 <- ap_all_n %>%
  arrange(-finger_pointing) %>%
  slice_head(n = 8) %>%
  select(finger_pointing, date, speaker, setting) %>%
  mutate(value = finger_pointing,
         Performance = "finger_pointing") %>%
  select(-finger_pointing)
common_8 <- ap_all_n %>%
  arrange(-common_sense) %>%
  slice_head(n = 8) %>%
  select(common_sense, date, speaker, setting) %>%
  mutate(value = common_sense,
         Performance = "common_sense") %>%
  select(-common_sense)
origins_8 <- ap_all_n %>%
  arrange(-origins) %>%
  slice_head(n = 8) %>%
  select(origins, date, speaker, setting) %>%
  mutate(value = origins,
         Performance = "origins") %>%
  select(-origins)
anti_pc_8 <- ap_all_n %>%
  arrange(-anti_pc) %>%
  slice_head(n = 8) %>%
  select(anti_pc, date, speaker, setting) %>%
  mutate(value = anti_pc,
         Performance = "anti_pc") %>%
  select(-anti_pc)
territory_8 <- ap_all_n %>%
  arrange(-territory) %>%
  slice_head(n = 8) %>%
  select(territory, date, speaker, setting) %>%
  mutate(value = territory,
         Performance = "territory") %>%
  select(-territory)
# Bind
ap_all_top8 <- rbind(truth_8, lie_8, consistency_8, fpoint_8,
                    common_8, origins_8, anti_pc_8, territory_8)
ap_all_top8$date2 <- stringr::str_extract(ap_all_top8$date, "[0-9]{2}$")

ggplot(ap_all_top8, aes(x = reorder(date2, as.numeric(date)),
                  y = value, fill = Performance,
                  label = speaker, shape = setting)) +
  geom_point(aes(shape = setting, color = Performance), size = 5) +
  geom_text(aes(label = speaker), size = 5, hjust = .5, vjust = -1) +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  labs(x = "",
       y = "",
       title = "Top Authenticity Performances by Speaker and Setting in Time for Brazil and the US",
       subtitle = "Top 8 performances per authenticity. Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight()
# But this is not really representative...
# Let's get only authenticity performaces bigger than 0.25
ap_all_top <- ap_all_n %>%
  select(-settingc) %>%
  tidyr::pivot_longer(consistency:anti_pc, "Performance") %>%
  arrange(-value) %>%
  filter(value > 0.25) %>%
  mutate(date2 = stringr::str_extract(date, "[0-9]{2}$"))

ggplot(ap_all_top, aes(x = reorder(date2, as.numeric(date)),
                       y = value, fill = Performance,
                       label = speaker, shape = setting)) +
  geom_point(aes(shape = setting, color = Performance), size = 5) +
  geom_text(aes(label = speaker), size = 5, hjust = 0.5, vjust = -.5) +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  labs(x = "",
       y = "",
       title = "Top Authenticity Performances by Speaker and Setting in Time for Brazil and the US",
       subtitle = "Authenticity performances with scores bigger than 0.25. Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight()
# Cool, but abit to confusing still.

# What if we split this for country and forget about setting?
ap_all_BR <- ap_all_n %>%
  mutate(case = stringr::str_sub(settingc, -2, -1)) %>%
  select(-settingc) %>%
  tidyr::pivot_longer(consistency:anti_pc, "Performance") %>%
  arrange(-value) %>%
  filter(case == "BR", value > 0.20) %>%
  select(-c(setting, case)) %>%
  group_by(date, speaker, Performance) %>%
  summarise(value = sum(value)) %>%
  mutate(date2 = stringr::str_extract(date, "[0-9]{2}$"))
# Get names right!!!
ap_all_BR$speaker <- gsub("Dilma", "Rousseff", ap_all_BR$speaker)
ap_all_BR$speaker <- gsub("Aecio", "Neves", ap_all_BR$speaker)
bb_sp <- ggplot(ap_all_BR, aes(x = reorder(date2, as.numeric(date)),
                       y = value, fill = Performance,
                       label = speaker)) +
  geom_point(aes(color = Performance), size = 4) +
  geom_text(aes(label = speaker), size = 5, vjust = 1) +
  labs(x = "",
       y = "",
       title = "Top Authenticity Performances by Speaker in Time for Brazil",
       subtitle = "Authenticity performances with scores bigger than 0.20. Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight() +
  guides(size = FALSE) +
  ylim(0,1)
bb_sp
# Same for the US
ap_all_US <- ap_all_n %>%
  mutate(case = stringr::str_sub(settingc, -2, -1)) %>%
  select(-settingc) %>%
  tidyr::pivot_longer(consistency:anti_pc, "Performance") %>%
  arrange(-value) %>%
  filter(case == "US", value > 0.20) %>%
  select(-c(setting, case)) %>%
  group_by(date, speaker, Performance) %>%
  summarise(value = sum(value)) %>%
  mutate(date2 = stringr::str_extract(date, "[0-9]{2}$"))

US_sp <- ggplot(ap_all_US, aes(x = reorder(date2, as.numeric(date)),
                               y = value, fill = Performance,
                               label = speaker)) +
  geom_point(aes(color = Performance), size = 4) +
  geom_text(aes(label = speaker), size = 5, vjust = 1) +
  labs(x = "",
       y = "",
       title = "Top Authenticity Performances by Speaker in Time for the US",
       subtitle = "Authenticity performances with scores bigger than 0.20. Normalized by number of characters per year in dataset") +
  theme_fivethirtyeight() +
  guides(size = FALSE) +
  ylim(0,1)
US_sp
# Plot side by side
gridExtra::grid.arrange(US_sp, bb_sp)

# Let's see how authenticity performances correlate amonsgt themselves
# BR
BR_corr <- BR_pp %>%
  mutate(truth_telling = (truth/length)*100000,
         lie_accusations = (lies/length)*100000,
         consistency = (consistency/length)*100000,
         finger_pointing = (fpoint/length)*100000,
         origins = (origins/length)*100000,
         common_sense = (common_sense/length)*100000,
         anti_pc = (anti_PC/length)*100000,
         territory = (territory/length)*100000) %>%
  select(-c(doc_id, setting, length, settingc, date, truth, lies, fpoint, anti_PC))
ap_cor_BR <- cor(BR_corr, method = "pearson", use = "everything")  # correlation tables for RP data
corrplot::corrplot(ap_cor_BR, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Super interesting!
# Let's get a table as well
ap_cor_BR2 <- rcorr(as.matrix(BR_corr))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
corr_table_BR <- flattenCorrMatrix(ap_cor_BR2$r, ap_cor_BR2$P)
knitr::kable(corr_table_BR, caption = " Correlation Table", format.args = list(scientific = FALSE), digits=4)
# US
US_corr <- US_pp %>%
  mutate(truth_telling = (truth/length)*100000,
         lie_accusations = (lies/length)*100000,
         consistency = (consistency/length)*100000,
         finger_pointing = (fpoint/length)*100000,
         origins = (origins/length)*100000,
         common_sense = (common_sense/length)*100000,
         anti_pc = (anti_PC/length)*100000,
         territory = (territory/length)*100000) %>%
  select(-c(doc_id, setting, length, settingc, date, truth, lies, fpoint, anti_PC))
ap_cor_US <- cor(US_corr, method = "pearson", use = "everything")  # correlation tables for RP data
corrplot::corrplot(ap_cor_US, type = "upper", order = "hclust",
                   tl.col = "black", tl.srt = 45)
# okay...
# Let's get a table as well
ap_cor_US2 <- rcorr(as.matrix(US_corr))
corr_table_US <- flattenCorrMatrix(ap_cor_US2$r, ap_cor_US2$P)
knitr::kable(corr_table_US, caption = " Correlation Table", format.args = list(scientific = FALSE), digits=4)
# Let's try to add the plots together
par(mfrow=c(1,2))
corrplot::corrplot(ap_cor_BR, type = "upper",
                   tl.col = "black", tl.srt = 60)
title("Brazil Authenticity Correlations")
corrplot::corrplot(ap_cor_US, type = "upper",
                   tl.col = "black", tl.srt = 60)
title("US Authenticity correlations")
par(mfrow=c(1,1))
