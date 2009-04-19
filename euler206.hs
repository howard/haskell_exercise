{-Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.-}



euler206 = head [x | x <- [1000000000,1000000001..], let s = show (x*x), length s == 19, s!!0=='1', s!!2=='2',
                                      s!!4=='3', s!!6=='4', s!!8=='5', s!!10=='6', s!!12=='7',
                                      s!!14=='8', s!!16=='9', s!!18=='0'] --correct 1389019170
