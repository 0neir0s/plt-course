# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [[[[0,0], [-1,0], [1,0], [2,0], [-2,0]],
                                [[0,0], [0,-1], [0,1], [0,2], [0,-2]]],
                              rotations([[0,0], [1,0], [0,1], [1,1], [2,1]]),
                              rotations([[0,0],[0,1],[1,1]])]

  Cheat_Piece = [[[0,0]]]
  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game 
    @delay = 500
    @cheat_enabled = false
  end 

  def cheat
    if @cheat_enabled or not @game.is_running? or (@score < 100)
      return
    end
    @score -= 100
    @cheat_enabled = true
  end 

  def next_piece
    if @cheat_enabled
      @current_block = MyPiece.new(MyPiece::Cheat_Piece, self)
    else
      @current_block = MyPiece.next_piece(self)
    end
    @cheat_enabled = false
    @current_pos = nil 
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }   
    remove_filled
    @delay = [@delay - 2, 80].max
  end 

end

class MyTetris < Tetris

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80) 
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc do @board.rotate_clockwise; @board.rotate_clockwise end )
    @root.bind('c', proc {@board.cheat})
  end
end
