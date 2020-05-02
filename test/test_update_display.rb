# @test
class Simple < Parent
  def method1
    return
  end

  def method2
    if true
      1
    elsif false
      2
    else
      3
    end

    for e in ['I do this'] do
      1
    end

    while true ; false || true
      1
      until (
        i ? i += 1 : i = 1
      ) > 2 do
        3
      end
    end

    10.times { |i|
      1
    }
  end
end

# @expect
class Simple < Parent
  def method1...end

  def method2
    if true...end

    for e in ['I do this'] do...end

    while true ; ...
      until (...end
    end

    10.times {...}
  end
end

