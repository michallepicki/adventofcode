#!/usr/bin/env ruby
twos = 0
threes = 0
File.open('../2', 'r') do |file|
  file.each do |id|
    counts = id.chars.group_by(&:itself).map{ |letter, list| list.length }
    if counts.include? 2 then twos += 1 end
    if counts.include? 3 then threes += 1 end
  end
end
puts twos * threes

require 'set'
patterns = Set[0]
found = false
File.open('../2', 'r') do |file|
  while not found and id = file.gets do
    for i in 0..(id.length-1)
      pattern = id.dup
      pattern[i] = '_'
      if not patterns.add?(pattern) then found = pattern.gsub('_', '') end
    end
  end
end
puts found
