#!/usr/bin/env ruby
twos = 0
threes = 0
File.open('../2', 'r') do |file|
  file.each do |id|
    counts = id.chars.group_by(&:itself).map { |_letter, list| list.length }
    twos += 1 if counts.include? 2
    threes += 1 if counts.include? 3
  end
end
puts twos * threes

require 'set'
patterns = Set[0]
found = false
File.open('../2', 'r') do |file|
  while !found && (id = file.gets)
    for i in 0..(id.length - 1)
      pattern = id.dup
      pattern[i] = '_'
      found = pattern.delete('_') unless patterns.add?(pattern)
    end
  end
end
puts found
