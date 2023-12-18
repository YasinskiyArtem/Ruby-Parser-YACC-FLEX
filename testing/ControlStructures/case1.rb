for i in (1..10)
    rno = rand(100) + 1
    msg = case rno
	when 42: 
		"The ultimate result."
	when 1..10: 
		"Way too small."
	when 11..15,19,27:
		 "Sorry, too small"
	when 80..99: 
		"Way to large"
	when 100:
		print "TOPS\n"
		"Really way too large"
	else "Just wrong"
    end
    print "Result: ", rno, ": ", msg, "\n"
end

puts "Что получили сегодня за урок?"
mark = gets.to_i

case mark
when 2
  puts "Это неуд"
when 3
  puts "Удовлетворительно"
when 4
  puts "Хорошо"
when 5
  puts "Отлично!"
else
  puts "Странная оценка"
end