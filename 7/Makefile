SOURCES = \
Ageable.ml \
Baratheon.ml \
CarbonBased.ml \
Dany.ml \
Direction.ml \
Dragon.ml \
Draw.ml \
Dust.ml \
Event51.ml \
Helpers.ml \
Human.ml \
KingsLanding.ml \
Main.ml \
Movable.ml \
Pond.ml \
Town.ml \
UI.ml \
Wall.ml \
WhiteWalker.ml \
World.ml \
WorldObject.ml \
WorldObjectI.ml

all: $(SOURCES)
	corebuild -quiet -lib graphics Main.native

check: $(SOURCES)
	@chmod u+x ../check_width
	@../check_width Ageable.ml; \
        ../check_width Baratheon.ml; \
	../check_width CarbonBased.ml; \
	../check_width Dany.ml; \
	../check_width Direction.ml; \
	../check_width Dragon.ml; \
	../check_width Draw.ml; \
	../check_width Dust.ml; \
	../check_width Event51.ml; \
	../check_width Helpers.ml; \
	../check_width Human.ml; \
	../check_width KingsLanding.ml; \
	../check_width Main.ml; \
	../check_width Movable.ml; \
	../check_width Pond.ml; \
	../check_width Town.ml; \
	../check_width UI.ml; \
	../check_width Wall.ml; \
	../check_width WhiteWalker.ml; \
	../check_width World.ml; \
	../check_width WorldObject.ml; \
	../check_width WorldObjectI.ml

clean:
	rm -rf _build Main.native
