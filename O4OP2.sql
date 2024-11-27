CREATE DATABASE ola4;

CREATE TABLE vw_cars (
    car_id INT PRIMARY KEY AUTO_INCREMENT,
    model VARCHAR(100),
    details TEXT,
    description TEXT,
    location VARCHAR(100),
    dealer_id INT,
    dealer_name VARCHAR(100),
    dealer_address TEXT,
    dealer_cvr VARCHAR(20),
    car_link TEXT
);

CREATE TABLE vw_prices (
    price_id INT PRIMARY KEY AUTO_INCREMENT,
    car_id INT,
    price DECIMAL(10, 2),
    date DATE NOT NULL,
    sold BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (car_id) REFERENCES vw_cars(car_id)
);

