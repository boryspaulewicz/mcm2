## -*- coding: utf-8 -*-

## Procedura: każda osoba ma oceniać i zapamiętać 30 przymiotników
## neg, neu i poz. Jedna z grup ocenia je ze względu na pewność, że
## sobie przypomną, druga grupa ze względu na afektywne
## skojarzenia. Czas prezentacji każdego słowa jest stały.
##
if(interactive())source('~/cs/code/r/tasks/task/task.R')
TASK.NAME <<- 'mcm2'

NOF.ITEMS = 10
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 5000
QUICK.SCALE = F ## czekamy, aż minie presentation time, zanim zniknie oceniane słowo

words = readRDS('slowa.RDS')

FX = fixation(WINDOW, size = .02)

scales = list(emotion = c('', 'Bardzo negatywne', 'Negatywne', 'Neutralne', 'Pozytywne', 'Bardzo pozytywne'),
              certainty = c('Czy to słowo wydaje się łatwe do zapamiętania?', 'Bardzo trudne', 'Trudne', 'Przeciętne', 'Łatwe', 'Bardzo łatwe'))

## Test pamięciowy - ocena walencji ze stałym czasem ekspozycji
mcm.trial.code = function(trial, word = 'test', samegender = 'same', scale = 'emotion'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        WINDOW$set.mouse.cursor.visible(T)
        state = 'press-space'
    }else{ state = 'show-fixation' }
    ## Ewentualna zmiana genderu słowa
    word = as.character(word)
    if(((samegender == 'same') && (USER.DATA$gender == 'K')) ||
       ((samegender != 'same') && (USER.DATA$gender == 'M'))){
        if(word == 'głupi'){
            word = 'głupia'
        }else{
            word = str_replace_all(word, 'y$', 'a')
            word = str_replace_all(word, 'i$', 'a')
        }
    }
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Proszę nacisnąć spację aby rozpocząć")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                scale.onset = CLOCK$time
                state = 'rating'
            }
        }, 'rating' = {
            WINDOW$clear(c(0, 0, 0))
            ## Rysujemy słowo
            TXT$set.string(word)
            ## Na wszelki wypadek
            TXT$set.color(c(1, 1, 1))
            center.win(TXT)## $move(c(0, WINDOW$get.size()[2] * -.2))
            WINDOW$draw(TXT)
            ## Pokazujemy skalę tylko dopóki nie zaznaczy odpowiedzi,
            ## albo nie minie maksymalny czas
            if(BUTTON.PRESSED[1] <= scale.onset){
                ## Pytanie dla skali (np. jak łatwo jest sobie wyobrazić...)
                TXT$set.string(scales[[as.character(scale)]][1])
                ## Na wszelki wypadek
                TXT$set.color(c(1, 1, 1))
                center.win(TXT)$move(c(0, WINDOW$get.size()[2] * .1))
                WINDOW$draw(TXT)
                value = draw.scale(scales[[as.character(scale)]][-1], position = .7)[1]
            }else if(QUICK.SCALE)state = 'done'
            ## Słowo pokazujemy do końca czasu pokazywania słowa, chyba, że QUICK.SCALE
            if((CLOCK$time - scale.onset) > PRESENTATION.TIME)state = 'done'
            WINDOW$display()
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(rating = value)
            return(res)
        })
    }
}

if(is.null(USER.DATA$name)){

    gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Wyłącz telefon komórkowy. W razie jakichkolwiek wątpliwości nie wołaj osoby prowadzącej, tylko podnieś do góry rękę.  Osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień.  Badanie jest anonimowe.

Za chwilę zostaniesz poproszona/y o podanie danych: wieku, płci oraz pseudonimu.  Pseudonim składa się z inicjałów (małymi literami) oraz czterech cyfr: dnia i miesiąca urodzenia (np.  ms0706). Proszę nie używać w inicjałach polskich znaków diakrytycznych (ą, ć, itd).")
gui.user.data() }

cnds = c('emotion', 'certainty')
if(USER.DATA$name == 'admin'){
    cnd = gui.choose.item(cnds)
}else{
    cnd = db.random.condition(cnds)
}

if(USER.DATA$name != 'admin'){

    gui.show.instruction("Teraz rozpocznie się etap polegający na wypełnieniu kilku kwestionariuszy. W każdym z kwestionariuszy prosimy zapoznać się z instrukcją.")
    
    ## PANAS-C
    
    gui.show.instruction('Skala, która się za chwilę pojawi, składa się ze słów nazywających różne emocje i uczucia. Przeczytaj każde słowo i zastanów się jak się czujesz ZAZWYCZAJ.')
    
    panas = gui.quest(c('aktywny(a)', '"jak na szpilkach"', 'mocny(a)', 'nerwowy(a)', 'ożywiony(a)', 'pełen (pełna) zapału', 'przerażony(a)', 'raźny(a)', 'silny(a)', 'winny(a)',
                        'wystraszony(a)', 'zalękniony(a)', 'zaniepokojony(a)', 'zapalony(a)', 'zawstydzony(a)', 'zdecydowany(a)', 'zdenerwowany(a)', 'zmartwiony(a)', 'żwawy(a)', 'żywy(a)'),
                      c('nieznacznie lub wcale', 'trochę', 'umiarkowanie', 'dość mocno', 'bardzo silnie'))
    
    ## CES-D
    
    gui.show.instruction(
        'Proszę zaznaczyć stwierdzenie, które najlepiej opisuje jak często czuł(a) się Pan/Pani lub zachowywał(a) w ten sposób w ciągu ostatniego tygodnia.
')
    
    cesd = gui.quest(c('Martwiły mnie rzeczy, które zazwyczaj mnie nie martwią.',
                       'Nie chciało mi się jeść, nie miałem(am) apetytu.',
                       'Czułem(am), że nie mogę pozbyć się chandry, smutku, nawet z pomocą rodziny i przyjaciół.',
                       'Wydawało mi się, że jestem gorszym człowiekiem niż inni ludzie.',
                       'Miałem(am) trudności ze skoncentrowaniem myśli na tym co robię.',
                       'Czułem(am) się przygnębiony(a).',
                       'Wszystko, co robiłem(am) przychodziło mi z trudem.',
                       'Patrzyłem(am) z nadzieją i ufnością w przyszłość.',
                       'Uważałem(am), że moje życie jest nieudane.',
                       'Czułem(am) lęk, obawy.',
                       'Żle sypiałem(am).',
                       'Czułem(am) się szczęśliwy(a).',
                       'Byłem(am) bardziej małomówny(a) niż zazwyczaj.',
                       'Czułem(am) się samotny(a).',
                       'Ludzie odnosili się do mnie nieprzyjaźnie.',
                       'Cieszyło mnie życie.',
                       'Miałem(am) napady płaczu.',
                       'Czułem(am) smutek.',
                       'Wydawało mi się, że ludzie mnie nie lubią.',
                       'Nic mi nie wychodziło.'),
                     c('< 1 dzień', '1-2 dni', '3-4 dni', '5-7 dni'))

} ## kwestionariusze dla nie-adminów

## Instrukcja przed etapem zapamiętywania

gui.show.instruction(sprintf("Teraz rozpocznie się zadanie wymagające zapamiętywania i oceny słów. Na ekranie komputera będą się pojawiały, jedno po drugim, różne słowa. Wszystkich słów będzie razem 30. UWAGA: każde słowo będzie wyświetlane nie dłużej niż kilka sekund.

Należy zaznaczyć za pomocą myszki, przyciskając lewy klawisz, %s

Samo położenie kursora myszki nie wystarczy, należy jeszcze potwierdzić ocenę klikając lewy przycisk myszki.

Należy starać się zapamiętywać wszystkie prezentowane i oceniane słowa, ponieważ na końcu badania będzie trzeba spróbować je sobie przypomnieć.",
ifelse(cnd == 'emotion',
       "na ile dane słowo Tobie osobiście kojarzy się negatywnie, neutralnie lub pozytywnie.

Pozycja kursora przy ocenie słów ma znaczenie - pozycja skrajnie z lewej strony oznacza maksymalnie negatywne skojarzenia, a pozycja skrajnie z prawej strony - maksymalnie pozytywne skojarzenia.",
       "czy dane słowo wydaje się Tobie osobiście łatwe, czy trudne do zapamiętania.

Pozycja kursora przy ocenie słów ma znaczenie - pozycja skrajnie z lewej strony oznacza, że słowo może być dla Ciebie bardzo trudne do zapamiętania, a pozycja skrajnie z prawej strony oznacza, że bardzo łatwo będzie Ci zapamiętać, że się pojawiło.")))

memset = sample(1:nrow(words), NOF.ITEMS)
run.trials(mcm.trial.code, expand.grid(scale = cnd, samegender = 'same',
                                   word = as.vector(as.matrix(words[memset,]))),
           record.session = T,
           condition = cnd)

######################################################################
## Zadanie dystrakcyjne - reagujemy lewo, prawo

## Globalne parametry zadania

MAX.REACTION.TIME = 3000
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW)
STIM = new(Text)
STIM$set.font(FONT)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    STIM$set.string(c(left = 'LEWO', right = 'PRAWO')[side])
    center.win(STIM)
    WINDOW$draw(STIM)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

leftright.trial.code = function(trial, side = 'left'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        WINDOW$set.mouse.cursor.visible(F)
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            CORRECT.KEY <<- c(left = Key.Left, right = Key.Right)[side]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME))state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            return(list(rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

gui.show.instruction("Teraz rozpocznie się zadanie wymagające szybkiego rozpoznawania słów.

Na środku ekranu będą się pojawiały słowa LEWO lub PRAWO.  Gdy tylko pojawi się słowo, należy nacisnąć odpowiednią strzałkę na klawiaturze.  Jeżeli będzie to słowo LEWO, należy nacisnąć klawisz STRZAŁKA W LEWO, a jeżeli słowo PRAWO, to strzałkę STRZAŁKA W PRAWO.

Program będzie rejestrował zarówno czas reakcji, jak i poprawność. Prosimy reagować możliwie szybko, ale poprawnie.

To zadanie potrwa około 3 minuty")

TASK.NAME <<- 'leftright'
run.trials(leftright.trial.code, condition = cnd, record.session = T, expand.grid(side = c('left', 'right')),
           max.time = 3 * 60000, b = 3 * 60)

gui.show.instruction("Prosimy teraz zapisać na kartce, z pamięci, w dowolnej kolejności, słowa, które pojawiały się na ekranie w zadaniu zapamiętywania i oceny słów. Etap odtwarzania słów będzie trwał około 3 minuty.  W tym czasie nic nie pojawi się na ekranie komputera.

UWAGA: Jeżeli wydaje Ci się, że słowo, które przychodzi Ci do głowy, pojawiło się wcześniej, ale nie jesteś tego pewny/a, napisz je. Później będzie można zaznaczyć, na ile jesteś pewny/a, że słowo faktywnie było prezentowane.

Po upłynięciu 3 minut od momentu naciśnięcia przycisku 'Dalej' ekran zacznie migotać, aby zasygnalizować koniec odtwarzania i przejście do następnego etapu.

Proszę nacisnąć przycisk 'Dalej' w dolnej części okna, aby rozpocząć etap odtwarzania słów z pamięci.")

WINDOW$set.visible(T)
recall.start = CLOCK$time
while((CLOCK$time - recall.start) < 3 * 60 * 1000){
    if(WINDOW$is.open())process.inputs()
    WINDOW$clear(c(0, 0, 0))
    WINDOW$display()
    if(KEY.PRESSED[Key.Escape + 1] > recall.start)break
}
## Migotanie
blinking.start = CLOCK$time
TXT$set.string("Koniec odtwarzania. Naciśnij spację.")
center.win(TXT)
while(WINDOW$is.open()){
    process.inputs()
    if(KEY.PRESSED[Key.Space + 1] > blinking.start){
        break
    }else{
        col = ceiling(CLOCK$time / 1000) %% 2
        TXT$set.color(c(1 - col, 1 - col, 1 - col))
        WINDOW$clear(c(col, col, col))
        WINDOW$draw(TXT)
        WINDOW$display()
    }
}
## Musimy się upewnić, że tekst jest na biało
TXT$set.color(c(1, 1, 1))
WINDOW$clear(c(0, 0, 0))
WINDOW$display()
WINDOW$set.visible(F)

gui.show.instruction("Teraz nastąpi kolejny etap zadania. Obok każdego słowa zapisanego na kartce proszę zaznaczyć, na ile jesteś pewna/pewien, że to słowo było prezentowane wcześniej w zestawie do zapamiętania.

1 oznacza, że nie jesteś W OGÓLE pewna/pewny

2 oznacza, że jesteś NIEZBYT pewna/pewny

3 oznacza, że jesteś RACZEJ pewna/pewny

4 oznacza, że jesteś CAŁKOWICIE pewna/pewny

Po zakończeniu oceny słów proszę nacisnąć przycisk Dalej, znajdujący się w dolnej części ekranu.")

gui.show.instruction("Proszę się upewnić, że obok każdego zapisanego słowa jest też liczba, która odpowiada temu, na ile jesteś pewny/a, że słowo to było prezentowane wcześniej w zestawie do zapamiętania. Przypominamy klucz odpowiedzi:

1 oznacza, że nie jesteś W OGÓLE pewna/pewny

2 oznacza, że jesteś NIEZBYT pewna/pewny

3 oznacza, że jesteś RACZEJ pewna/pewny

4 oznacza, że jesteś CAŁKOWICIE pewna/pewny

Jeżeli nie każde słowo zostało w ten sposób oznaczone, proszę uzupełnić brakujące informacje. Jeżeli wszystkie słowa są w ten sposób oznaczone, można nacisnąć przycisk Dalej, znajdujący się w dolnej części ekranu.")

######################################################################
## Zapamiętujemy dane kwestionariuszowe

if(USER.DATA$name != 'admin'){
    db.connect()
    panas = as.list(panas)
    names(panas) = paste('i', 1:length(panas), sep = '')
    db.create.data.table(panas, 'mcm2_panas')
    panas$session_id = SESSION.ID
    db.insert.data(panas, 'mcm2_panas')
    cesd = as.list(cesd)
    names(cesd) = paste('i', 1:length(cesd), sep = '')
    db.create.data.table(cesd, 'mcm2_cesd')
    cesd$session_id = SESSION.ID
    db.insert.data(cesd, 'mcm2_cesd')
    db.disconnect()
}

######################################################################
## Etap rozpoznawania

gui.show.instruction("Teraz Twoim zadaniem będzie rozpoznać, czy różne słowa były, czy nie były prezentowane w zestawie do zapamiętania. Na ekranie komputera znowu będą się pojawiały, jedno po drugim, różne słowa. Każde słowo będzie wyświetlane tak długo, aż udzielisz odpowiedzi.

Należy zaznaczyć za pomocą myszki, przyciskając lewy klawisz, czy dane słowo było prezentowane wcześniej i na ile jesteś pewien/pewna, że było.

Pozycja kursora przy ocenie słów ma znaczenie - pozycja skrajnie z lewej strony oznacza, że jesteś całkowicie pewien/pewna, że słowo nie było prezentowane, pozycja na środku, że nie potrafisz powiedzieć, czy było, czy nie, a pozycja skrajnie z prawej strony, że jesteś całkowicie pewien/pewna, że słowo było.

Samo położenie kursora myszki nie wystarczy, należy jeszcze potwierdzić ocenę klikając lewy przycisk myszki.")

NOF.ITEMS = 10
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 60 * 1000
QUICK.SCALE = T ## Nie czekamy aż minie presentation time
scales = list(retro = c('', 'Na pewno nie było', 'Raczej nie było', 'Nie wiem', 'Raczej było', 'Na pewno było'))
## Tutaj dajemy wszystkie stare i taką samą liczbę nowych bodźców
memset2 = sample(c(memset, sample((1:nrow(words))[-memset], NOF.ITEMS)))
TASK.NAME <<- 'mcm2_recognition'
run.trials(mcm.trial.code, expand.grid(scale = 'retro', samegender = 'same',
                                       word = as.vector(as.matrix(words[memset2,]))),
           record.session = T,
           condition = cnd)

gui.show.instruction('Dziękujemy za udział w badaniu.

Proszę poczekać na swoim miejscu, aż osoba prowadząca badanie podejdzie i udzieli dalszych instrukcji.')

## Koniec
if(!interactive())quit("no")
