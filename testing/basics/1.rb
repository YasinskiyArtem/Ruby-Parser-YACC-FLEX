def sayHello(word="Ничего", num=0)
    puts "Привет мир!"
    puts ("Ваше слово: " + word + " и ваше число: " + num.to_s)
  end
#  
  sayHello
#  
  def summa(x, y)
    return x + y, 70
  end

def my_method
    yield 5
    puts "Продолжаем выполнять my_method"
    yield 7
  end
#  
  my_method { |count| puts "Выполняем блок с count равным #{count}" }

def my_method
    yield 5, 'Саша'
    puts "Продолжаем выполнять my_method"
    yield 7, 'Миша'
  end
#  
  my_method do |count, name|
    puts "Выполняем блок с count равным #{count} и name равным #{name}"
  end

# in lib/resolv-replace.rb

class SOCKSSocket < TCPSocket
    # :stopdoc:
    #alias original_resolv_initialize initialize
    # :startdoc:
    def initialize(host, serv)
      original_resolv_initialize(IPSocket.getaddress(host), port)
    end
  end if defined? SOCKSSocket