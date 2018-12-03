#!/usr/bin/env ruby
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
  until found
    file.seek(0)
    while !found && (freq_change = file.gets)
      freq += Integer(freq_change)
      found = true unless reached_freqs.add?(freq)
    end
  end
end
puts freq
