# frozen_string_literal: true

require_relative '../lib/chess'
require 'ruby-prof'

include ChessH

RSpec.describe ChessH do
    context "coord" do
        it { expect(a8).to eq 21 }
        it { expect(h8).to eq 28 }
        it { expect(a1).to eq 91 }
        it { expect(h1).to eq 98 }
    end
end

describe String do
    context "to_idx" do
        it { expect("a8".to_idx).to eq 21 }
        it { expect("h8".to_idx).to eq 28 }
        it { expect("a1".to_idx).to eq 91 }
        it { expect("h1".to_idx).to eq 98 }
    end
end

describe Fixnum do
    context "to_sq" do
        it { expect(21.to_sq).to eq "a8" }
        it { expect(28.to_sq).to eq "h8" }
        it { expect(91.to_sq).to eq "a1" }
        it { expect(98.to_sq).to eq "h1" }
    end
end

describe Array do
    context "to_idx" do
        it { expect([0,0].to_idx).to eq a8 }
        it { expect([1,0].to_idx).to eq b8 }
        it { expect([0,1].to_idx).to eq a7 }
    end
end

describe Spot do
    context ".new" do
        it { expect(subject.board[e4]).to eq nil }
        it { expect(subject.board[e2]).to eq :P }
        it { expect(subject.board[d1]).to eq :Q }
        it { expect(subject.board[d8]).to eq :q }
        it { expect(subject.turn).to eq :white }
        it { expect(subject.ep).to eq nil }
        it { expect(subject.castling).to eq 'KQkq' }
        it { expect(subject.halfmove).to eq 0 }
        it { expect(subject.fullmove).to eq 1 }
        it { expect(subject.king).to eq white: e1, black: e8 }
    end
    context ".[]" do
        it { expect(Spot[R: e4]).to eq Spot[R: e4] }
        it { expect(Spot[R: e4].board[e4]).to eq :R }
        it { expect(Spot[R: [e2,e4]].board[e2]).to eq :R }
        it { expect(Spot[R: [e2,e4]].board[e4]).to eq :R }
        it { expect(Spot[K: e1, R: h1]).to eq Spot[K: e1, R: h1, castling: "K"] }
    end
    context "#move" do
        context "e4" do
            subject { Spot.new.move("e4") }
            it { expect(subject.board[e4]).to eq :P }
            it { expect(subject.board[e2]).to eq nil }
            it { expect(subject.turn).to eq :black }
            it { expect(subject.ep).to eq e3 }
            it { expect(subject.castling).to eq 'KQkq' }
            it { expect(subject.halfmove).to eq 0 }
            it { expect(subject.fullmove).to eq 1 }
        end
        context "e4 e5" do
            subject { Spot.new.move("e4").move("e5") }
            it { expect(subject.board[e5]).to eq :p }
            it { expect(subject.board[e7]).to eq nil }
            it { expect(subject.turn).to eq :white }
            it { expect(subject.ep).to eq e6 }
            it { expect(subject.castling).to eq 'KQkq' }
            it { expect(subject.halfmove).to eq 0 }
            it { expect(subject.fullmove).to eq 2 }
        end
        context "e4 e5 Nf3" do
            subject { Spot.new.move("e4").move("e5").move("Nf3") }
            it { expect(subject.board[f3]).to eq :N }
            it { expect(subject.board[g1]).to eq nil }
            it { expect(subject.turn).to eq :black }
            it { expect(subject.ep).to eq nil }
            it { expect(subject.castling).to eq 'KQkq' }
            it { expect(subject.halfmove).to eq 1 }
            it { expect(subject.fullmove).to eq 2 }
        end
        context "e4 e5 Nf3 Nf6" do
            subject { Spot.new.move("e4").move("e5").move("Nf3").move("Nf6") }
            it { expect(subject.board[f6]).to eq :n }
            it { expect(subject.board[g8]).to eq nil }
            it { expect(subject.turn).to eq :white }
            it { expect(subject.ep).to eq nil }
            it { expect(subject.castling).to eq 'KQkq' }
            it { expect(subject.halfmove).to eq 2 }
            it { expect(subject.fullmove).to eq 3 }
        end
        context "e4 e5 Nf3 Nf6 Nxe5" do
            subject { Spot.new.move("e4").move("e5").move("Nf3").move("Nf6").move("Nxe5") }
            it { expect(subject.board[e5]).to eq :N }
            it { expect(subject.board[f3]).to eq nil }
            it { expect(subject.turn).to eq :black }
            it { expect(subject.ep).to eq nil }
            it { expect(subject.castling).to eq 'KQkq' }
            it { expect(subject.halfmove).to eq 0 }
            it { expect(subject.fullmove).to eq 3 }
        end
        context "read moves from pgn" do
            it {
                #RubyProf.start
                File.open("Morphy.pgn", "r") do |f|
                    spot = Spot.new
                    game = 1
                    while line = f.gets
                        next if line.start_with?("[")

                        line.gsub(/\b\d+\./, '').split.each do |str|
                            case str
                            when %r"^1-0$|^0-1$|^1/2-1/2$|^\*$" then
                                spot = Spot.new
                                game += 1
                            else
                                spot.move(str)
                                #puts '', "Game #{game}",spot, str
                            end
                        end
                    end
            end
            #RubyProf::FlatPrinter.new(RubyProf.stop).print(STDOUT)
            }
        end
        context "two args" do
            it { expect(Spot[R: e4].move(e4, e2)).to eq Spot[R: e2, :turn => :black, :halfmove => 1] }
            it "removes castling if king is moved" do
                 expect(Spot[K: e1, R: [a1, h1]].move(e1, e2)).to eq Spot[K: e2, R: [a1, h1], :castling => "", :turn => :black, :halfmove => 1]
            end
            it "castles kingside" do
                expect(Spot[K: e1, R: h1].move(e1, g1)).to eq Spot[K: g1, R: f1, :castling => "", :turn => :black, :halfmove => 1]
            end
            it "castles queenside" do
                expect(Spot[K: e1, R: a1].move(e1, c1)).to eq Spot[K: c1, R: d1, :castling => "", :turn => :black, :halfmove => 1]
            end
            it "handles ep" do
                expect(Spot[P: e5, p: f5, :ep => f6 ].move(e5, f6)).to eq Spot[P: f6, :turn => :black ]
            end
        end
        context "pawn promotion" do
            it {expect(Spot[P: e7].move(e7, e8, :Q)).to eq Spot[Q: e8, :turn => :black] }
        end
    end
    context "#move_str" do
        it { expect(Spot[R: e4].move_str(e4,e2)).to eq "Re2" }
        it { expect(Spot[R: [e4, a4]].move_str(e4, b4)).to eq "Reb4" }
        it { expect(Spot[R: [e4, e2]].move_str(e4, e3)).to eq "R4e3" }
        it { expect(Spot[B: [d3, d5, f3, f5]].move_str(d3, e4)).to eq "Bd3e4" }
        it { expect(Spot[P: e2].move_str(e2, e4)).to eq "e4" }
        it { expect(Spot[P: e2, p: d3].move_str(e2, d3)).to eq "ed" }
        it { expect(Spot[P: [e2, e3], p: [d3, d4]].move_str(e2, d3)).to eq "ed3" }
        it { expect(Spot[K: e1].move_str(e1, e2)).to eq "Ke2" }
        it { expect(Spot[K: e1, R: h1].move_str(e1, g1)).to eq "O-O" }
        it { expect(Spot[K: e1, R: a1].move_str(e1, c1)).to eq "O-O-O" }
    end
    context "#poss_moves" do
        it "allows rook moves" do 
            expect(Spot[R: e4].poss_moves_str).to eq \
                ["Ra4", "Rb4", "Rc4", "Rd4", "Re8", "Re7", "Re6", "Re5", "Re3", "Re2", "Re1", "Rf4", "Rg4", "Rh4"] 
        end
        it "allows bishop moves" do 
            expect(Spot[B: e4].poss_moves_str).to eq \
                ["Ba8", "Bb7", "Bb1", "Bc6", "Bc2", "Bd5", "Bd3", "Bf5", "Bf3", "Bg6", "Bg2", "Bh7", "Bh1"] 
        end
        it "allows king moves and castle" do
            expect(Spot[K: e1, R: h1].poss_moves_str.sort).to eq\
            ["Kd1", "Kd2", "Ke2", "Kf1", "Kf2", "O-O", "Rf1", "Rg1", "Rh2", "Rh3", "Rh4", "Rh5", "Rh6", "Rh7", "Rh8"]
        end
        it "allows king moves and long castle" do 
            expect(Spot[K: e1, R: a1].poss_moves_str.sort).to eq\
            ["Kd1", "Kd2", "Ke2", "Kf1", "Kf2", "O-O-O", "Ra2", "Ra3", "Ra4", "Ra5", "Ra6", "Ra7", "Ra8", "Rb1", "Rc1", "Rd1"]
        end
        it "allows piece capture" do
             expect(Spot[R: e4, r: e6].poss_moves_str.sort).to eq  ["Ra4", "Rb4", "Rc4", "Rd4", "Re1", "Re2", "Re3", "Re5", "Re6", "Rf4", "Rg4", "Rh4"]
        end
        it "allows knight moves" do
            expect(Spot[N: e4].poss_moves_str.sort).to eq ["Nc3", "Nc5", "Nd2", "Nd6", "Nf2", "Nf6", "Ng3", "Ng5"]
            expect(Spot[N: a1].poss_moves_str.sort).to eq ["Nb3", "Nc2"]
        end
        it "allows queen moves" do
            expect(Spot[q: e4, :turn => :black].poss_moves_str).to eq \
                ["Qa8", "Qa4", "Qb7", "Qb4", "Qb1", "Qc6", 
                "Qc4", "Qc2", "Qd5", "Qd4", "Qd3", "Qe8", 
                "Qe7", "Qe6", "Qe5", "Qe3", "Qe2", "Qe1", 
                "Qf5", "Qf4", "Qf3", "Qg6", "Qg4", "Qg2",
                "Qh7", "Qh4", "Qh1"]
        end
        it "can't castle in check" do
            expect(Spot[K: e1, R: h1, r: e8].poss_moves_str).not_to include "O-O"
        end
        it "can't castle through check" do
            expect(Spot[K: e1, R: h1, r: f8].poss_moves_str).not_to include "O-O"
        end
        it "can't leave the king in check" do
            expect(Spot[K: e1, R: h1, r: e8].poss_moves_str).not_to include "Rh2"
        end
        it "castles through a clear path" do
            expect(Spot[k: e8, r: a8, B: b8, :turn => :black ].poss_moves_str).not_to include "O-O-O" 
        end
        it "allows pawn moves" do
            expect(Spot[P: e3].poss_moves_str).to eq ["e4"]
            expect(Spot[P: e2].poss_moves_str.sort).to eq ["e3","e4"]
            expect(Spot[P: e3, p: e4].poss_moves_str).to eq []
            expect(Spot[P: e2, p: e3].poss_moves_str.sort).to eq []
        end
        it "allows pawn capture" do
            expect(Spot[P: e2, p: d3].poss_moves_str).to include "ed"
        end
        it "allows ep capture" do
            expect(Spot[P: e5, p: d5, :ep => d6].poss_moves_str).to eq  ["ed", "e6"]
        end
        it "allows pawn promotion" do
            expect(Spot[P: e7].poss_moves_str.sort).to eq ["e8=B", "e8=N", "e8=Q", "e8=R"]
        end
        it "allows pawn promotion on capture" do
            expect(Spot[P: e7, r: d8].poss_moves_str.sort).to eq ["e8=B", "e8=N", "e8=Q", "e8=R", "ed=B", "ed=N", "ed=Q", "ed=R"]

        end
    end

    context "#checkm?" do
        it { expect(Spot.new.checkm?).to eq false }
        it { expect(Spot[K: e1, q: e2, k: e3].checkm?).to eq true }
        it { expect(Spot[K: e1, q: e2, k: e8].checkm?).to eq false }
    end
    context "#stalem?" do
        it { expect(Spot.new.stalem?).to eq false }
        it { expect(Spot[K: e1, p: e2, k: e3].stalem?).to eq true }
        it { expect(Spot[K: e1, p: e2, k: e8].stalem?).to eq false }
    end

    context "#minmax" do
        it { expect(Spot[K: e1, q: e2, k: e3].minmax).to eq (-100) }
        it { expect(Spot[k: e1, Q: e2, K: e3, :turn => :black].minmax).to eq 100 }
        it { expect(Spot[K: e1, p: e2, k: e3].minmax).to eq 0 }
        it { expect(Spot[K: e1, q: d3, k: e3, :turn => :black].minmax).to eq (-99) }
        it { expect(Spot[k: e1, Q: d3, K: e3, :turn => :white].minmax).to eq 99 }
    end

    context "#best_mv" do
        it { expect(Spot[K: e1, q: d3, k: e3, b: b1, :turn => :black].best_mv).to eq [d3, e2] }
    end
    context "#endgame?" do
        it { expect(Spot.new.endgame?).to eq false }
        it { expect(Spot[k: e1, Q: e2, K: e3, :turn => :black].endgame?).to eq true }
        it { expect(Spot[K: e1, p: e2, k: e3].endgame?).to eq true }
    end
end