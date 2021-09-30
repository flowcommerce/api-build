#!/usr/bin/env ruby

def readFiles(dir)
  Dir.glob("#{dir}/*").map { |f| File.basename(f) }
end

def diff(file)
  a = File.join("main/#{file}")
  b = File.join("explore/#{file}")
  if File.exists?(a) && File.exists?(b)
    output = "#{file}.diff.txt"
    `diff #{a} #{b} > #{output}`
    if File.read(output).to_s.strip.empty?
      File.delete(output)
      "Identical"
    else
      "Has differences"
    end
  else
    nil
  end
end

files = (readFiles("./explore") + readFiles("./main")).uniq

all = {}
files.each do |f|
  if changes = diff(f)
    all[changes] ||= []
    all[changes] << f
  end
end

all.keys.sort.each do |key|
  puts ""
  puts key
  all[key].sort.each do |d|
    puts " - #{d}"
  end
end

puts ""
