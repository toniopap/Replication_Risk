function onFormSubmit(e) {
  
  var form = FormApp.openById('1olpgbqAFOrSS5dRDWKJUjT4oOMQOQMdv-ibHdOE_d20'); //Open the form by its ID
   // Get the last response
  var responsesCount = form.getResponses().length;
  var responses = form.getResponses(); 
  var response = responses[responsesCount-1];
  var responseItems = response.getItemResponses();
  var recipient = response.getRespondentEmail(); // Get the email address of the respondent
  var initial_endowment = 500
  // Random Lottery extraction
  var lotteryNr = Math.floor((Math.random() * 33) + 1); // Generate a random integer number between 1 and 33
  // Initialize variables for the lottery series, switch point, row number, and probabilities
  let serieNr, switchPoint, rowNr, yellowProbA, yellowProbB;
  // Determine the series number (1,2 or 3), the row and probabilities based on the lotteryNr (1-33)
  if (lotteryNr <= 12){
    serieNr = 1;
    switchPoint = responseItems[4].getResponse();
    rowNr = lotteryNr;
    yellowProbA = 0.3; // 30% chance for yellow in lottery A
    yellowProbB = 0.1; // 10% chance for yellow in lottery B
  } else if (lotteryNr <= 26) {
    serieNr = 2;
    switchPoint = responseItems[5].getResponse();
    rowNr = lotteryNr - 12;
    yellowProbA = 0.9; // 90% chance for yellow in lottery A
    yellowProbB = 0.7; // 70% chance for yellow in lottery B
  } else  {
    serieNr = 3;
    switchPoint = responseItems[6].getResponse();
    rowNr = lotteryNr - 26;
    yellowProbA = 0.5; // 50% chance for yellow in lottery A
    yellowProbB = 0.5; // 50% chance for yellow in lottery B
  }
  // Convert the switchPoint response to a number if it is a string
  if (switchPoint == '0 I would never choose lottery A') {
    switchPoint = 0;
  }
  if (switchPoint == '14 (always A)') {
    switchPoint = 14;
  }
  if (switchPoint == '12 (always lottery A)') {
    switchPoint = 12;
  }  
  if (switchPoint == '7 (always A)') {
    switchPoint = 7;
  }
  // Determine the user's choice based on the row number and switch point
  var userChoice = (rowNr <= switchPoint) ? 'A' : 'B';
  // Extract the color of the drawn ball based on the user's choice and the probabilities
  var extraction = Math.random();
  var ballColor = (userChoice == 'A') ? (extraction < yellowProbA ? 'yellow' : 'white') : (extraction < yellowProbB ? 'yellow' : 'white');
  //Array with the gains 
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
  var total_gain = initial_endowment + gain
  var gainMoney = total_gain / 100; // Convert gain to money (assuming gain is in cents)
  // Log the results for debugging
  Logger.log('Lottery Number: ' + lotteryNr);
  Logger.log('Row Number: ' + rowNr);
  Logger.log('Serie Number: ' + serieNr);
  Logger.log('User Choice: ' + userChoice);
  Logger.log('Extraction: ' + extraction);
  Logger.log('Ball Color: ' + ballColor);
  Logger.log('Gain: ' + gain);
  Logger.log('Total gain: ' + total_gain)

  var emailaddress = 'tonio.paparella@gmail.com'; 
  var subject = 'Lottery outcome'; // Replace with your desired subject
  var message = 'The random number generator selected Row number ' + rowNr +' from Serie '+ serieNr + '. In this Row, you have selected Lottery ' + userChoice+'. The ball that has been drawn is ' + ballColor +'. This means you gained '+ gain + ' Points. The total amount you will receive is '+ total_gain + '.';
  Logger.log(message);
  // Send the email directly

  //MailApp.sendEmail(recipient, subject, message);
  // Send a copy of the email to yourself
  var self_message = recipient + ' has submitted the form. The random number generator selected Row number ' + rowNr +' from Serie '+ serieNr + '. In this Row, the user has selected Lottery ' + userChoice+'. The ball that has been drawn is ' + ballColor +'. This means the user gained '+ gain + ' Points. The total amount the user will receive is '+ total_gain + '.';
  Logger.log(self_message);
  //MailApp.sendEmail(emailaddress, subject, self_message);
}
