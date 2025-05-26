function onFormSubmit(e) {
  
  var form = FormApp.openById('1olpgbqAFOrSS5dRDWKJUjT4oOMQOQMdv-ibHdOE_d20');
  var responsesCount = form.getResponses().length;
  var responses = form.getResponses();
  var response = responses[responsesCount-1];
  var responseItems = response.getItemResponses();
  var recipient = response.getRespondentEmail();
  
  // Random Lottery extraction
  var lotteryNr = Math.floor((Math.random() * 33) + 1); 
  var rowNr = 0;
  var yellowProbA = 0;
  var yellowProbB = 0;
  if (lotteryNr <= 12){
    var serieNr = 1;
    var switchPoint = responseItems[4].getResponse();
    var rowNr = lotteryNr;
    var yellowProbA = 0.3; // 30% chance for yellow in lottery A
    var yellowProbB = 0.1; // 10% chance for yellow in lottery B
  } else if (lotteryNr <= 26) {
    var serieNr = 2;
    var switchPoint = responseItems[5].getResponse();
    var rowNr = lotteryNr - 12;
    var yellowProbA = 0.9; // 90% chance for yellow in lottery A
    var yellowProbB = 0.7; // 70% chance for yellow in lottery B
  } else  {
    var serieNr = 3;
    var switchPoint = responseItems[6].getResponse();
    var rowNr = lotteryNr - 26;
    var yellowProbA = 0.5; // 50% chance for yellow in lottery A
    var yellowProbB = 0.5; // 50% chance for yellow in lottery B
  }

  if (switchPoint == '0 I would never choose lottery A') {
    var switchPoint = 0;
  }
  if (switchPoint == '14 (always A)') {
    var switchPoint = 14;
  }
  
  if (switchPoint == '12 (always lottery A)') {
    var switchPoint = 12;
  }  
  if (switchPoint == '7 (always A)') {
    var switchPoint = 7;
  }
  // Determina la scelta implicita dell’utente nella riga estratta
  var userChoice = (rowNr <= switchPoint) ? 'A' : 'B';
  // Determina il colore della pallina estratta
  var extraction = Math.random() ;
  var ballColor = (userChoice == 'A') ? (extraction < yellowProbA ? 'yellow' : 'white') : (extraction < yellowProbB ? 'yellow' : 'white');
  //array with the gains 
  var outcomesA = {
    1: [[400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100], [400, 100]],
    2: [[400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300], [400, 300]],
    3: [[250, -40], [40, -40], [10, -40], [10, -40], [10, -80], [10, -80], [10, -80]]
  };

  var outcomesB = {
    1: [[680, 50], [750, 50], [830, 50], [930, 50], [1060, 50], [1250, 50], [1500, 50], [1850, 50], [2200, 50], [3000, 50], [4000, 50], [6000, 50]],
    2: [[540, 50], [560, 50], [580, 50], [600, 50], [620, 50], [650, 50], [680, 50], [720, 50], [770, 50], [830, 50], [900, 50], [1000, 50], [1100, 50], [1300, 50]],
    3: [[300, -210], [300, -210], [300, -210], [300, -160], [300, -160], [300, -140], [300, -110]]
  };
  // Calculate the gain based on the user's choice and the drawn ball color
  var gain = (userChoice == 'A') ? (ballColor == 'yellow') ? outcomesA[serieNr][rowNr -1][0] : outcomesA[serieNr][rowNr - 1][1] : (ballColor == 'yellow') ? outcomesB[serieNr][rowNr - 1][0] : outcomesB[serieNr][rowNr - 1][1];
  var gainMoney = gain / 100; // Convert gain to money (assuming gain is in cents)
  // Log the results for debugging
  Logger.log('Lottery Number: ' + lotteryNr);
  Logger.log('Row Number: ' + rowNr);
  Logger.log('Serie Number: ' + serieNr);
  Logger.log('User Choice: ' + userChoice);
  Logger.log('Extraction: ' + extraction);
  Logger.log('Ball Color: ' + ballColor);
  Logger.log('Gain: ' + gain);
  // Extracting individual responses
  //var nameResponse = responseItems[0].getResponse();

  var emailaddress = 'tonio.paparella@gmail.com'; 
  var subject = 'Lottery outcome'; // Replace with your desired subject
  var message = 'The random number generator selected Row number ' + rowNr +' from Serie '+ serieNr + '. In this Row, you have selected Lottery ' + userChoice+'.  The ball that has been drawn is ' + ballColor +'. This means you gained '+ gain + ' Points. The total amount you will receive is '+ gainMoney + ' Euros.';
  Logger.log(message);
  // Send the email directly

  //MailApp.sendEmail(recipient, subject, message);

}
