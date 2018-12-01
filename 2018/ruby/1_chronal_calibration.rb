#! /usr/bin/env ruby
freq = 0
File.open('../1', 'r') do |file|
  file.each do |freq_change|
    freq += Integer(freq_change)
  end
end
puts freq

require 'set'
freq = 0
reached_freqs = Set[0]
File.open('../1', 'r') do |file|
  found = false
  while not found do
    file.seek(0)
    while not found and freq_change = file.gets do
      freq += Integer(freq_change)
      if not reached_freqs.add?(freq)
        found = true
      end
    end
  end
end
puts freq