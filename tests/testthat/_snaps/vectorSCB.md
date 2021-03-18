# vectorSCB works

    Code
      vectorSCB(vectorColumn = LandUseType, Municipality = "Ludvika")
    Output
      bebyggd mark och tillhörande mark                         betesmark 
                                   4774                               226 
               golfbanor och skidpistar            skogsmark, improduktiv 
                                     86                             23852 
                   skogsmark, produktiv                   total landareal 
                                 106468                            149037 
                        total skogsmark            täkter och gruvområden 
                                 130320                               629 
                                 vatten                          åkermark 
                                  15744                               747 
                             övrig mark 
                                  12255 

---

    Code
      vectorSCB(vectorColumn = LandUseType, Municipality = "Ludvika", forceTable = "LandUse")
    Output
      bebyggd mark och tillhörande mark                         betesmark 
                                   4774                               226 
               golfbanor och skidpistar            skogsmark, improduktiv 
                                     86                             23852 
                   skogsmark, produktiv                   total landareal 
                                 106468                            149037 
                        total skogsmark            täkter och gruvområden 
                                 130320                               629 
                                 vatten                          åkermark 
                                  15744                               747 
                             övrig mark 
                                  12255 

---

    Code
      vectorSCB(vectorColumn = LandUseType, Municipality = "Ludvika", verbose = TRUE)
    Output
      bebyggd mark och tillhörande mark                         betesmark 
                                   4774                               226 
               golfbanor och skidpistar            skogsmark, improduktiv 
                                     86                             23852 
                   skogsmark, produktiv                   total landareal 
                                 106468                            149037 
                        total skogsmark            täkter och gruvområden 
                                 130320                               629 
                                 vatten                          åkermark 
                                  15744                               747 
                             övrig mark 
                                  12255 

---

    Code
      vectorSCB(vectorColumn = LandUseType, Municipality = "Ludvika", forceTable = "LandUse",
        verbose = TRUE)
    Output
      bebyggd mark och tillhörande mark                         betesmark 
                                   4774                               226 
               golfbanor och skidpistar            skogsmark, improduktiv 
                                     86                             23852 
                   skogsmark, produktiv                   total landareal 
                                 106468                            149037 
                        total skogsmark            täkter och gruvområden 
                                 130320                               629 
                                 vatten                          åkermark 
                                  15744                               747 
                             övrig mark 
                                  12255 

---

    Code
      vectorSCB(vectorColumn = LandUseType, LandUseType = c("Total forest",
        "Farmland"), Municipality = "Ludvika", forceTable = "LandUse", verbose = TRUE)
    Output
      Total forest     Farmland 
            130320          747 

---

    Code
      vectorSCB(Gender = "Male", forceTable = "AgeGender", vectorColumn = "Municipality")
    Output
                  Ale        Alingsås         Alvesta           Aneby          Arboga 
                16054           20646           10339            3498            7114 
             Arjeplog      Arvidsjaur          Arvika       Askersund          Avesta 
                 1458            3182           13064            5788           11734 
           Bengtsfors            Berg        Bjurholm            Bjuv           Boden 
                 5051            3652            1249            8053           14266 
            Bollebygd         Bollnäs        Borgholm        Borlänge           Borås 
                 4801           13453            5433           26635           56606 
             Botkyrka         Boxholm        Bromölla          Bräcke          Burlöv 
                48481            2829            6555            3320            9668 
               Båstad         Dals-Ed        Danderyd       Degerfors         Dorotea 
                 7528            2518           16163            4901            1356 
                  Eda           Ekerö           Eksjö        Emmaboda        Enköping 
                 4387           14616            8912            4868           22987 
           Eskilstuna           Eslöv         Essunga        Fagersta      Falkenberg 
                53628           17138            2842            6798           22842 
            Falköping           Falun       Filipstad        Finspång            Flen 
                16724           29442            5448           11241            8503 
             Forshaga      Färgelanda          Gagnef        Gislaved          Gnesta 
                 5839            3459            5243           15382            5718 
               Gnosjö         Gotland           Grums        Grästorp       Gullspång 
                 5043           29816            4578            2895            2667 
            Gällivare           Gävle        Göteborg          Götene            Habo 
                 9103           51067          290308            6746            6290 
              Hagfors       Hallsberg   Hallstahammar        Halmstad         Hammarö 
                 5997            8178            8208           51487            8302 
              Haninge       Haparanda            Heby        Hedemora     Helsingborg 
                46922            4993            7157            7877           73329 
           Herrljunga             Hjo          Hofors        Huddinge      Hudiksvall 
                 4824            4609            4925           57236           18899 
            Hultsfred           Hylte       Hällefors      Härjedalen       Härnösand 
                 7337            5548            3613            5279           12601 
              Härryda      Hässleholm            Håbo         Höganäs          Högsby 
                19140           26282           11101           13378            3070 
                Hörby            Höör        Jokkmokk        Järfälla       Jönköping 
                 7932            8406            2546           40559           70816 
                Kalix          Kalmar       Karlsborg       Karlshamn       Karlskoga 
                 8176           34614            3544           16381           15222 
           Karlskrona        Karlstad     Katrineholm             Kil           Kinda 
                34259           46651           17432            6125            5039 
               Kiruna         Klippan         Knivsta        Kramfors    Kristianstad 
                11925            9082            9568            9423           42829 
         Kristinehamn          Krokom           Kumla      Kungsbacka         Kungsör 
                12232            7729           10887           42053            4457 
              Kungälv        Kävlinge          Köping          Laholm      Landskrona 
                23189           15901           13286           13173           23191 
                 Laxå        Lekeberg         Leksand           Lerum         Lessebo 
                 2954            4286            8008           21404            4487 
              Lidingö       Lidköping      Lilla Edet      Lindesberg       Linköping 
                23551           20107            7367           12013           83183 
              Ljungby         Ljusdal    Ljusnarsberg           Lomma         Ludvika 
                14579            9592            2463           12385           13663 
                Luleå            Lund        Lycksele         Lysekil           Malmö 
                39961           61946            6204            7326          170623 
         Malung-Sälen            Malå       Mariestad            Mark        Markaryd 
                 5262            1586           12389           17575            5327 
             Mellerud          Mjölby            Mora          Motala         Mullsjö 
                 4834           14078           10342           22140            3658 
             Munkedal        Munkfors         Mölndal       Mönsterås      Mörbylånga 
                 5454            1887           34864            6785            7568 
                Nacka            Nora         Norberg      Nordanstig      Nordmaling 
                52260            5318            2917            4884            3654 
           Norrköping       Norrtälje          Norsjö           Nybro         Nykvarn 
                71745           31688            2068           10356            5627 
             Nyköping       Nynäshamn          Nässjö         Ockelbo       Olofström 
                28126           14505           16151            3013            6873 
                 Orsa           Orust            Osby      Oskarshamn        Ovanåker 
                 3538            7726            6631           13788            6021 
            Oxelösund          Pajala        Partille        Perstorp           Piteå 
                 6079            3222           19802            3852           21316 
              Ragunda     Robertsfors         Ronneby         Rättvik            Sala 
                 2716            3468           15099            5509           11554 
                Salem       Sandviken         Sigtuna      Simrishamn           Sjöbo 
                 8304           19821           24972            9463            9814 
                Skara      Skellefteå Skinnskatteberg          Skurup          Skövde 
                 9459           36943            2257            7990           28572 
         Smedjebacken       Sollefteå      Sollentuna           Solna         Sorsele 
                 5577            9680           37094           41252            1319 
              Sotenäs    Staffanstorp     Stenungsund       Stockholm        Storfors 
                 4622           12589           13463          482220            2108 
             Storuman       Strängnäs       Strömstad       Strömsund      Sundbyberg 
                 3028           18410            6740            6044           26461 
            Sundsvall           Sunne      Surahammar          Svalöv         Svedala 
                49917            6776            5187            7475           11159 
           Svenljunga          Säffle           Säter          Sävsjö       Söderhamn 
                 5537            7792            5713            5985           12947 
          Söderköping      Södertälje      Sölvesborg           Tanum           Tibro 
                 7420           50623            8812            6553            5713 
             Tidaholm           Tierp           Timrå        Tingsryd           Tjörn 
                 6488           10796            9154            6360            8146 
            Tomelilla          Torsby          Torsås         Tranemo          Tranås 
                 6871            5922            3708            6239            9619 
           Trelleborg     Trollhättan           Trosa          Tyresö            Täby 
                22794           29767            6886           24254           35763 
             Töreboda       Uddevalla      Ulricehamn            Umeå  Upplands Väsby 
                 4810           28404           12552           64654           23524 
         Upplands-Bro         Uppsala      Uppvidinge        Vadstena        Vaggeryd 
                14925          114333            5005            3697            7292 
         Valdemarsvik      Vallentuna         Vansbro            Vara         Varberg 
                 4023           17191            3483            8221           32397 
              Vaxholm        Vellinge        Vetlanda      Vilhelmina        Vimmerby 
                 5943           18125           14088            3414            8022 
              Vindeln        Vingåker      Vänersborg          Vännäs          Värmdö 
                 2777            4656           19992            4530           22750 
              Värnamo       Västervik        Västerås           Växjö        Vårgårda 
                17525           18407           77311           47666            6031 
                 Ydre           Ystad         Älmhult        Älvdalen      Älvkarleby 
                 1934           14946            9029            3615            4832 
              Älvsbyn       Ängelholm            Åmål            Ånge             Åre 
                 4167           20880            6343            4846            6083 
               Årjäng           Åsele          Åstorp      Åtvidaberg          Öckerö 
                 5172            1456            8201            5857            6486 
              Ödeshög          Örebro     Örkelljunga    Örnsköldsvik       Östersund 
                 2740           77228            5325           28361           31491 
            Österåker       Östhammar    Östra Göinge       Överkalix      Övertorneå 
                23132           11447            7745            1737            2234 

