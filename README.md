# Mašininio mokymosi metodų pritaikomumas prognozuojant makroekonominių rodiklių laiko eilutes
Šiuolaikiniame pasaulyje makroekonominių rodiklių prognozavimas yra labai svarbi 
ekonometrijos sritis, kuri leidžia tiek politikams, tiek verslininkams priimti pagrįstus strateginius 
sprendimus. Šiame darbe analizuojamas mašininio mokymosi metodų taikymas Lietuvos BVP prognozavimui ir jų palyginimas su tradiciniais ekonominiais modeliais. Tyrimo pradžioje surinkti ir paruošti ilgos trukmės makroekonominiai duomenys iš patikimų šaltinių, tokių kaip Eurostat ir Statistikos departamentas. Taip pat apžvelgti tradiciniai prognozavimo metodai ir jų skirtumai nuo mašininio mokymosi algoritmų. Praktinėje dalyje yra sukurti ir palyginti tiesiniai modeliai, mašininio mokymosi metodai su slenkančiu langu bei neuroninis tinklas TACTiS-2, siekiant įvertinti prognozių tikslumą bei jautrumą ekonominiams šokams - krizėms. Galiausiai, aptartos galimos tyrimo pratęsimo kryptys.

# Duomenys
Kursiniam darbui reikalingi duomenys yra prieinami per Google Drive:
https://drive.google.com/drive/folders/1KJkwcrIYoj76-Kh9U96yczlGoHkUndbf?usp=sharing

# RStudio failai
- duomenu_tvarkyba - čia vyksta pirminis duomenų surinkimas ir apdorojimas: NA šalinimas, stacionarumo tikrinimas bei taisymas, normalizavimas. Gale pateiktas bendras BVP grafikas.
- paprasciausi_modeliai - čia vyksta kintamųjų atrinkimas bei buvo išnagrinėti paprasčiausi prognozavimo modeliai, tarp kurių ARIMA, tiesinė regresija, LASSO, Ridge, Elastic Net, Adaptive LASSO bei Random Forest modeliai. 
- slenkancio_lango_modeliai - čia buvo testuojami tokie slenkančio lango modeliai: Post–LASSO su tiesine regresija, Post–LASSO su LASSO, Random Forest su tarpusavio informacija, Random Forest su LASSO, Random Forest su Random Forest, XGBOOST (paprastas ir tiesinis metodas) su Random Forest, XGBOOST (paprastas ir tiesinis metodas) su LASSO, XGBOOST (paprastas ir tiesinis metodas) su XGBOOST bei Random Forest su XGBOOST.

# tactis2.ipynb failas
Šiame faile yra TACTiS-2 kodas, kuris atlieka aprašytus kursiniame darbe veiksmus.

# stacionarus rinkinys.xlsx
Šiame faile yra aprašytas darbe stacionarus rinkinys.

# nestacionarus rinkinys.xlsx
Šiame faile yra aprašytas darbe nestacionarus rinkinys.
