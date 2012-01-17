rows = %w(A B C D E F G H I)
cols = %w(1 2 3 4 5 6 7 8 9)

xs =  rows.map do |r|
  cols.map { |c| "#{r}#{c}" }
end

p xs
