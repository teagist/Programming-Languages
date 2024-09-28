#  Hangman
#
#  The game begins by clearing the screen and displaying the number of 
#  blanks along with the instructions and a hint for the word.  The
#  user will enter single letter guess until they successfully guess
#  the word or run out of "lives".  As the user guesses correctly the empty
#  spaces will be filled in with the letters of the word.  However, as the
#  user guesses incorrectly, the hangman will be filled out.  The game will
#  end once the user has fully filled out the word, ran out of lives, or
#  types 'exit'.


class Hangman

    def initialize
        @word = words.sample
        @lives = 7
        @word_teaser = ""
        @word.first.size.times do
            @word_teaser += "_ "
        end
    end


    def words
        [
            ["cricket", "A gentlemen's sport"],
            ["mason", "Someone who is good with stone"],
            ["zombie", "walkers, lurkers, biters, and other undead"],
            ["galaxy", "A long time ago, in a..."],
            ["halloween", "Free candy for everyone"],
        ]
    end


    def print_teaser last_guess = nil
        update_teaser(last_guess) unless last_guess.nil?
        puts @word_teaser
    end


    # Updating the word teaser with the user's guess
    def update_teaser last_guess
        new_teaser = @word_teaser.split
        new_teaser.each_with_index do |letter, index|
            # replace blank values with guessed letter if matches letter in word
            if letter == '_' && @word.first[index] == last_guess
                new_teaser[index] = last_guess
            end
        end
        @word_teaser = new_teaser.join(' ')
    end


    def print_man
        if @lives == 7
            puts " ___"
            puts "|"
            puts "|"
            puts "|"
            puts "|"
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 6
            puts " ___"
            puts "|   |"
            puts "|   "
            puts "|"
            puts "|"
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 5
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|"
            puts "|"
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 4
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|   |"
            puts "|"
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 3
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|  -|"
            puts "|"
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 2
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|  -|-"
            puts "|   "
            puts "|______"
            puts "|______|"
            puts ""

        elsif @lives == 1
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|  -|-"
            puts "|  /"
            puts "|______"
            puts "|______|"
            puts ""
        end

    end


    # Take in the user's guess.
    # If it is correct, then update the word with the letter.
    # If it is incorrect, then update the hangman
    def make_guess
        if @lives > 0
            puts "Enter a letter"
            guess = gets.chomp

            # if letter is not part of word then remove from letters array
            good_guess = @word.first.include? guess

            if guess == "exit"
                puts "Thanks for playing!"

            # Good guess => Fill in the letter
            elsif good_guess
                Gem.win_platform? ? (system "cls") : (system "clear")
                print_man
                print_teaser guess

                if @word.first == @word_teaser.split.join
                    puts "Good job, you got it!"
                else
                    make_guess
                end

            # Bad guess => Decrease the lives and print the new hangman
            else
                @lives -= 1
                Gem.win_platform? ? (system "cls") : (system "clear")
                print_man
                print_teaser guess
                puts "Sorry... you have #{ @lives } guesses left."
                make_guess
            end

        # Out of guesses => Game Over!
        else
            puts " ___"
            puts "|   |"
            puts "|   O"
            puts "|  -|-"
            puts "|  / \\  "
            puts "|______"
            puts "|______|"
            puts "Game Over! Better luck next time."
        end
    end


    # Clear the screen to start a new game and print the appropriate
    # number of blanks along with the instructions and a hint
    def begin
        # Clearing the screen with the appropriate system command
        Gem.win_platform? ? (system "cls") : (system "clear")

        #Instructions and game setup
        puts "Your word is #{ @word.first.size} characters long"
        puts "To quit the game, type 'exit'"
        print_man
        print_teaser

        puts "Hint: #{ @word.last }"
        make_guess

    end
end


game = Hangman.new
game.begin
