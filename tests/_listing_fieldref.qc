entity ()                         spawn     = #3;

float TICK_LENGTH = 0.016;

.float health;
.float energy;

void regenerate_resource(entity player, .float res, float dt) {
    player.(res) += 0.5 * dt;
}

void main() {
    entity player = spawn();
    regenerate_resource(player, health, TICK_LENGTH);
    regenerate_resource(player, energy, TICK_LENGTH);
}
