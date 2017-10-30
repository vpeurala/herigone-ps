#!/usr/bin/env ruby
yarn_lock_file_name = ARGV[0]

lines = File.readlines(yarn_lock_file_name)

lines.each_with_index {|value, index|
  match_data = /"@bower_components\/[^#]*#([^"]*)":/.match(value.chomp)
  lines[index] = value.chomp
  lines[index + 1] = "  version \"#{match_data[1]}\"" if match_data
}

File.open(yarn_lock_file_name, "w") { |f|
  f.write(lines.join("\n") + "\n")
}
