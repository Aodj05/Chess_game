# frozen_string_literal: false

require "gosu"
require_relative "./lib/chess.rb"

class Chess < Gosu::Window
    attr_accessor :width, :height, :cellw, :spot, :fnt, :from, :to, :player
    
    PIECES = {R: '♖', N: '♘', B: '♗', Q: '♕', K: '♔', P: '♙', r: '♜', n: '♞', b: '♝', q: '♛', k: '♚', p: '♟︎'}
    def initialize
        @cellw = 100
        @width = cellw*8
        @height = cellw*8
        super(width, height, false)
        @spot = Spot.new
        @fnt = Gosu::Font.new(self, Gosu::default_font_name, cellw) 
    end

    def needs_cursor?
        true
    end

    def get_idx
        [mouse_x.to_i/cellw, mouse_y.to_i/cellw].to_idx
    end

    def button_down(id)
        close if id == Gosu::KbQ
        @from = get_idx if id == Gosu::MsLeft
    end

    def button_up(id)
        if id == Gosu::MsLeft
            @to = get_idx
            if spot.poss_moves.include?([from, to])
                spot.move(from, to)
                @player = :computer_wait
            end
        end
    end

    def update
        case player
        when :computer_wait then @player = :computer
        when :computer 
            if !spot.endgame?
                puts "Computer begin"
                best = spot.best_mv
                spot.move(*best)
                puts "computer ends"
            end
            @player = :human
        end
    end

    def draw
        x = 0
        y = 0
        light = Gosu::Color::argb(0xff, 0xc0, 0xc0, 0xc0)
        dark = Gosu::Color::argb(0xff, 138, 3, 3)
        (0..7).each do |i|
            (0..7).each do |j|
                color = (i+j)%2 == 0 ? light : dark
                x = i * cellw
                y = j * cellw
                draw_quad(x, y, color,
                    x+cellw, y, color,
                    x+cellw, y+cellw, color,
                    x, y+cellw, color)
                piece = spot.board[[i, j].to_idx]
                str = PIECES[piece] || ''
                cx = (cellw - fnt.text_width(str))/2
                fnt.draw(str, x + cx, y, 1, 1, 1, Gosu::Color::BLACK)
            end
        end
    end
end

chess = Chess.new
chess.show
